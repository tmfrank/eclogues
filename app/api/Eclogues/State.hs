{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Eclogues.State (createJob, updateJobs, getJob, getJobs, activeJobs, killJob, deleteJob) where

import Eclogues.API (JobError (..))
import Eclogues.Scheduling.Command (ScheduleCommand (..))
import Eclogues.State.Monad (TS)
import qualified Eclogues.State.Monad as ES
import Eclogues.State.Types (AppState, Jobs, jobs)
import Eclogues.JobSpec ( Name, FailureReason (..), RunErrorReason (..)
                        , JobSpec
                        , JobState (..), isActiveState, isTerminationState
                                       , isExpectedTransition, isOnScheduler
                        , JobStatus (JobStatus), QueueStage (LocalQueue) )
import qualified Eclogues.JobSpec as Job

import Control.Lens ((^.), view)
import Control.Monad (when, void)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans.Writer.Lazy (WriterT, tell, execWriterT)
import Data.HashMap.Lazy (elems, traverseWithKey)
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (isJust)
import Data.Monoid (Sum (Sum))
import Data.UUID (UUID)

createJob :: forall m. (TS m, MonadError JobError m) => UUID -> JobSpec -> m ()
createJob uuid spec = do
    let name = spec ^. Job.name
        deps = spec ^. Job.dependsOn
    existing <- ES.getJob name
    when (isJust existing) $ throwError JobNameUsed
    Sum activeDepCount <- execWriterT $ mapM_ (checkDep name) deps
    let jstate = if activeDepCount == 0
            then Queued LocalQueue
            else Waiting activeDepCount
    ES.insertJob $ JobStatus spec jstate uuid
    if jstate == Queued LocalQueue
        then ES.schedule $ QueueJob spec uuid
        else pure ()
    where
        checkDep :: Name -> Name -> WriterT (Sum Integer) m ()
        checkDep name depName = ES.getJob depName >>= \case
            Just ds -> case ds ^. Job.jobState of
                Finished            -> ES.addRevDep depName name
                s | isActiveState s -> tell 1 *> ES.addRevDep depName name
                  | otherwise       -> throwError $ JobCannotHaveFailed depName
            Nothing -> throwError $ JobMustExist depName

existingJob :: (TS m, MonadError JobError m) => Name -> m JobStatus
existingJob name = ES.getJob name >>= \case
    Just js -> pure js
    Nothing -> throwError NoSuchJob

killJob :: (TS m, MonadError JobError m) => Name -> m ()
killJob name = existingJob name >>= \js ->
    if isTerminationState (js ^. Job.jobState)
        then throwError $ JobMustBeTerminated False
        else do
            ES.setJobState name Killing
            ES.schedule $ KillJob name (js ^. Job.uuid)

deleteJob :: (TS m, MonadError JobError m) => Name -> m ()
deleteJob name = existingJob name >>= \js -> do
    when (isActiveState $ js ^. Job.jobState) . throwError $ JobMustBeTerminated True
    dependents <- ES.getDependents name
    when (not $ null dependents) . throwError $ OutstandingDependants dependents
    ES.deleteJob name
    ES.schedule $ CleanupJob name (js ^. Job.uuid)

-- Only check on active jobs; terminated jobs shouldn't change status
-- Also Waiting jobs aren't in Aurora so filter out those
activeJobs :: AppState -> Jobs
activeJobs = HashMap.filter (isNonWaitingActiveState . view Job.jobState) . view jobs where
    isNonWaitingActiveState :: JobState -> Bool
    isNonWaitingActiveState (Waiting _) = False
    isNonWaitingActiveState s           = isActiveState s

updateJobs :: forall m. (TS m) => Jobs -> [(Name, JobState)] -> m ()
updateJobs activeStatuses gotStates = void $ traverseWithKey transition activeStatuses where
    transition :: Name -> JobStatus -> m ()
    transition name pst =
        let oldState = pst ^. Job.jobState
            newState = checkTransition oldState $ lookup name gotStates
        in when (newState /= oldState) $ do
            ES.setJobState name newState
            handleDeps pst newState
    checkTransition :: JobState -> Maybe JobState -> JobState
    checkTransition Killing Nothing    = Failed UserKilled
    checkTransition Killing (Just new)
        | Finished <- new              = Failed UserKilled
        | isTerminationState new       = new
        | otherwise                    = Killing
    checkTransition old     Nothing
        | isOnScheduler old            = RunError SchedulerLost
        | otherwise                    = old
    checkTransition old     (Just new)
        | old == new                   = old
        | isExpectedTransition old new = new
        | otherwise                    = RunError BadSchedulerTransition
    handleDeps :: JobStatus -> JobState -> m ()
    handleDeps pst newState
        | isTerminationState newState = do
            let name = pst ^. Job.name
            rdepNames <- ES.getDependents name
            -- Remove this job from the rev deps of its dependencies
            mapM_ (flip ES.removeRevDep name) $ pst ^. Job.dependsOn
            case newState of
                Finished -> mapM_ triggerDep rdepNames
                _        -> mapM_ (flip ES.setJobState . Failed $ DependencyFailed name) rdepNames
        | otherwise                  = pure ()
    triggerDep :: Name -> m ()
    triggerDep rdepName = ES.getJob rdepName >>= \case
        Just (JobStatus spec (Waiting 1) uuid) -> do
            ES.schedule $ QueueJob spec uuid
            ES.setJobState rdepName $ Queued LocalQueue
        Just (JobStatus _    (Waiting n) _   ) ->
            ES.setJobState rdepName $ Waiting $ n - 1
        _         -> pure ()

getJobs :: AppState -> [JobStatus]
getJobs = elems . view jobs

getJob :: (MonadError JobError m) => Name -> AppState -> m JobStatus
getJob name state = case HashMap.lookup name (state ^. jobs) of
    Nothing -> throwError NoSuchJob
    Just js -> pure js
