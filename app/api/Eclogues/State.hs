{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
                        getJob, getJobs, activeJobs, pendingJobs
                      -- * Mutate
                      , createJob, updateJobStates, updateJobSatis, killJob, deleteJob) where

import Eclogues.API (JobError (..))
import Eclogues.Monitoring.Cluster (Cluster, satisfiability)
import Eclogues.Scheduling.Command (ScheduleCommand (..))
import Eclogues.State.Monad (TS)
import qualified Eclogues.State.Monad as ES
import Eclogues.State.Types (AppState, Jobs, jobs)
import Eclogues.Job (
      FailureReason (..), RunErrorReason (..), QueueStage (LocalQueue), Stage (..), Satisfiability(..)
    , isActiveStage, isTerminationStage, isExpectedTransition, isOnScheduler, isPendingStage)
import qualified Eclogues.Job as Job

import Control.Lens ((^.), view)
import Control.Monad (when, void, unless)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans.Writer.Lazy (WriterT, tell, execWriterT)
import Data.Bool (bool)
import Data.HashMap.Lazy (elems, traverseWithKey)
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (isJust, catMaybes)
import Data.Monoid (Sum (Sum))
import Data.UUID (UUID)

-- | Try to schedule a new job. UUID must be unique; best to randomly generate
-- it.
createJob :: forall m. (TS m, MonadError JobError m) => UUID -> Maybe Cluster -> Job.Spec -> m ()
createJob uuid cluster spec = do
    let name = spec ^. Job.name
        deps = spec ^. Job.dependsOn
        satis = maybe SatisfiabilityUnknown (`satisfiability` spec) cluster
    existing <- ES.getJob name
    when (isJust existing) $ throwError JobNameUsed
    Sum activeDepCount <- execWriterT $ mapM_ (checkDep name) deps
    let jstage = if activeDepCount == 0
            then Queued LocalQueue
            else Waiting activeDepCount
    ES.insertJob $ Job.Status spec jstage satis uuid
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
activeJobs = filterJobsByStage isNonWaitingActiveStage where
    isNonWaitingActiveStage :: Job.Stage -> Bool
    isNonWaitingActiveStage (Waiting _) = False
    isNonWaitingActiveStage s           = isActiveStage s

-- | All jobs that are waiting to run.
pendingJobs :: AppState -> Jobs
pendingJobs = filterJobsByStage isPendingStage

filterJobsByStage :: (Job.Stage -> Bool) -> AppState -> Jobs
filterJobsByStage f = HashMap.filter (f . view Job.stage) . view jobs

-- | Update the status of a set of jobs with new data from the scheduler,
-- scheduling any new actions required (eg. dependencies).
updateJobStates :: forall m. (TS m) => Jobs -> [(Job.Name, Job.Stage)] -> m ()
updateJobStates activeStatuses gotStages = void $ traverseWithKey transition activeStatuses where
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
            mapM_ (`ES.removeRevDep` name) $ pst ^. Job.dependsOn
            case newStage of
                Finished -> mapM_ triggerDep rDepNames
                _        -> do
                    rDepStatuses <- catMaybes <$> mapM ES.getJob rDepNames
                    mapM_ (cascadeDepFailure name) rDepStatuses
        | otherwise                  = pure ()
    triggerDep :: Job.Name -> m ()
    triggerDep rdepName = ES.getJob rdepName >>= \case
        Just (Job.Status spec (Waiting 1) _ uuid) -> do
            ES.schedule $ QueueJob spec uuid
            ES.setJobStage rdepName $ Queued LocalQueue
        Just (Job.Status _    (Waiting n) _ _) ->
            ES.setJobStage rdepName $ Waiting $ n - 1
        _         -> pure ()
    cascadeDepFailure :: Job.Name -> Job.Status -> m ()
    cascadeDepFailure depName cst = do
        let name = cst ^. Job.name
        rDepNames <- ES.getDependents name
        mapM_ (`ES.removeRevDep` name) $ cst ^. Job.dependsOn
        ES.setJobStage name (Failed $ DependencyFailed depName)
        rDepStatuses <- catMaybes <$> mapM ES.getJob rDepNames
        mapM_ (cascadeDepFailure name) rDepStatuses

-- | Update the satisfiability of a set of jobs.
updateJobSatis :: forall m. (TS m) => Jobs -> [(Job.Name, Satisfiability)] -> m ()
updateJobSatis activeStatuses gotSatis = void $ traverseWithKey satis activeStatuses
    where
        satis name jobStatus = case lookup name gotSatis of
            Just x -> bool (ES.setJobSatis name x) (pure ()) $ jobStatus ^. Job.satis == x
            Nothing -> pure ()

-- | All jobs.
getJobs :: AppState -> [Job.Status]
getJobs = elems . view jobs

-- | Retrieve a job status by name.
getJob :: (MonadError JobError m) => Job.Name -> AppState -> m Job.Status
getJob name state = case HashMap.lookup name (state ^. jobs) of
    Nothing -> throwError NoSuchJob
    Just js -> pure js
