{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Job(s) state views and transformation. Actions are actually scheduled in 'TS'.
-}

module Eclogues.State (
    -- * View
      getJob, getJobs, activeJobs
    -- * Mutate
    , createJob, updateJobs, killJob, deleteJob
    ) where

import Eclogues.API (JobError (..))
import Eclogues.Job (
      FailureReason (..), RunErrorReason (..), QueueStage (..), Stage (..)
    , isActiveStage, isTerminationStage, isOnScheduler, isQueueStage)
import qualified Eclogues.Job as Job
import Eclogues.Monitoring.Cluster (Cluster, stagelessSatisfy)
import Eclogues.Scheduling.Command (ScheduleCommand (..))
import qualified Eclogues.State.Monad as ES
import Eclogues.State.Monad (TS)
import Eclogues.State.Types (AppState, Jobs, jobs)

import Control.Lens ((^.), view)
import Control.Monad (when, void, unless)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Writer.Lazy (WriterT, tell, execWriterT)
import Data.Foldable (traverse_)
import Data.HashMap.Lazy (elems, traverseWithKey)
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (catMaybes, isJust)
import Data.Monoid (Sum (Sum))
import Data.UUID (UUID)

-- TODO: Determine satisfiability before scheduling job
-- | Try to schedule a new job. UUID must be unique; best to randomly generate
-- it.
createJob :: forall m. (TS m, MonadError JobError m) => UUID -> Maybe Cluster -> Job.Spec -> m ()
createJob uuid cluster spec = do
    let name = spec ^. Job.name
        deps = spec ^. Job.dependsOn
    existing <- ES.getJob name
    when (isJust existing) $ throwError JobNameUsed
    Sum activeDepCount <- execWriterT $ traverse_ (checkDep name) deps
    satis <- stagelessSatisfy cluster spec
    let jstage = if activeDepCount == 0
            then Queued LocalQueue
            else Waiting activeDepCount
        jStatus = Job.mkStatus spec jstage satis uuid
    ES.insertJob jStatus
    if jstage == Queued LocalQueue
        then ES.schedule $ QueueJob spec uuid
        else pure ()
    where
        checkDep :: Job.Name -> Job.Name -> WriterT (Sum Integer) m ()
        checkDep name depName = ES.getJob depName >>= \case
            Just ds -> case ds ^. Job.stage of
                Finished            -> ES.addRevDep depName name
                s | isActiveStage s -> tell 1 *> ES.addRevDep depName name
                  | otherwise       -> throwError $ JobCannotHaveFailed depName
            Nothing -> throwError $ JobMustExist depName

-- | Retrieve an existing job.
existingJob :: (TS m, MonadError JobError m) => Job.Name -> m Job.Status
existingJob name = ES.getJob name >>= \case
    Just js -> pure js
    Nothing -> throwError NoSuchJob

-- | Kill a job. Fails if job is already terminated.
killJob :: (TS m, MonadError JobError m) => Job.Name -> m ()
killJob name = existingJob name >>= \js ->
    if isTerminationStage (js ^. Job.stage)
        then throwError $ JobMustBeTerminated False
        else do
            ES.setJobStage name Killing
            ES.schedule $ KillJob name (js ^. Job.uuid)

-- | Delete a terminated job and all its output.
deleteJob :: (TS m, MonadError JobError m) => Job.Name -> m ()
deleteJob name = existingJob name >>= \js -> do
    when (isActiveStage $ js ^. Job.stage) . throwError $ JobMustBeTerminated True
    dependents <- ES.getDependents name
    unless (null dependents) . throwError $ OutstandingDependants dependents
    ES.deleteJob name
    ES.schedule $ CleanupJob name (js ^. Job.uuid)

-- Only check on active jobs; terminated jobs shouldn't change status
-- Also Waiting jobs aren't in Aurora so filter out those
-- Can't use 'isOnScheduler' because 'LocalQueue' jobs may have been scheduled.
-- | All jobs that should be queried on the scheduler.
activeJobs :: AppState -> Jobs
activeJobs = HashMap.filter (isNonWaitingActiveStage . view Job.stage) . view jobs where
    isNonWaitingActiveStage :: Job.Stage -> Bool
    isNonWaitingActiveStage (Waiting _) = False
    isNonWaitingActiveStage s           = isActiveStage s

-- | Update the status of a set of jobs with new data from the scheduler,
-- scheduling any new actions required (eg. dependencies).
updateJobs :: forall m. (TS m) => Jobs -> [(Job.Name, Job.Stage)] -> m ()
updateJobs activeStatuses gotStages = void $ traverseWithKey transition activeStatuses where
    transition :: Job.Name -> Job.Status -> m ()
    transition name pst =
        let oldStage = pst ^. Job.stage
            newStage = checkTransition oldStage $ lookup name gotStages
        in when (newStage /= oldStage) $ do
            ES.setJobStage name newStage
            handleDeps pst newStage
    checkTransition :: Job.Stage -> Maybe Job.Stage -> Job.Stage
    checkTransition Killing Nothing    = Failed UserKilled
    checkTransition Killing (Just new)
        | Finished <- new              = Failed UserKilled
        | isTerminationStage new       = new
        | otherwise                    = Killing
    checkTransition old     Nothing
        | isOnScheduler old            = RunError SchedulerLost
        | otherwise                    = old
    checkTransition old     (Just new)
        | old == new                   = old
        | isExpectedTransition old new = new
        | otherwise                    = RunError BadSchedulerTransition
    handleDeps :: Job.Status -> Job.Stage -> m ()
    handleDeps pst newStage
        | isTerminationStage newStage = do
            let name = pst ^. Job.name
            rDepNames <- ES.getDependents name
            -- Remove this job from the rev deps of its dependencies
            traverse_ (`ES.removeRevDep` name) $ pst ^. Job.dependsOn
            case newStage of
                Finished -> traverse_ triggerDep rDepNames
                _        -> do
                    rDepStatuses <- catMaybes <$> traverse ES.getJob rDepNames
                    traverse_ (cascadeDepFailure name) rDepStatuses
        | otherwise                  = pure ()
    triggerDep :: Job.Name -> m ()
    triggerDep rdepName = (ES.getJob rdepName >>=) . traverse_ $ \st ->
        case st ^. Job.stage of
            Waiting 1 -> do
                ES.schedule $ QueueJob (st ^. Job.spec) (st ^. Job.uuid)
                ES.setJobStage rdepName $ Queued LocalQueue
            Waiting n ->
                ES.setJobStage rdepName . Waiting $ n - 1
            _         -> pure ()
    cascadeDepFailure :: Job.Name -> Job.Status -> m ()
    cascadeDepFailure depName cst = do
        let name = cst ^. Job.name
        rDepNames <- ES.getDependents name
        traverse_ (`ES.removeRevDep` name) $ cst ^. Job.dependsOn
        ES.setJobStage name (Failed $ DependencyFailed depName)
        rDepStatuses <- catMaybes <$> traverse ES.getJob rDepNames
        traverse_ (cascadeDepFailure name) rDepStatuses

-- | All jobs.
getJobs :: AppState -> [Job.Status]
getJobs = elems . view jobs

-- | Retrieve a job status by name.
getJob :: (MonadError JobError m) => Job.Name -> AppState -> m Job.Status
getJob name state = case HashMap.lookup name (state ^. jobs) of
    Nothing -> throwError NoSuchJob
    Just js -> pure js

-- | Whether the first 'Stage' may transition to the second in the lifecycle.
-- Unexpected transitions are treated as scheduler errors
-- ('BadSchedulerTransition').
isExpectedTransition :: Stage -> Stage -> Bool
isExpectedTransition (Queued LocalQueue) (Queued SchedulerQueue) = True
isExpectedTransition (Queued _)   Running       = True
isExpectedTransition (Waiting 0)  Running       = True
isExpectedTransition Running      (Queued SchedulerQueue) = True
isExpectedTransition Killing      (Failed UserKilled) = True
isExpectedTransition o n | isQueueStage o || o == Running = case n of
                                  Finished     -> True
                                  (Failed _)   -> True
                                  (RunError _) -> True
                                  _            -> False
isExpectedTransition _            _             = False
