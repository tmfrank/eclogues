{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Eclogues.State (createJob, updateJobs, getJob, getJobs, activeJobs, killJob, deleteJob) where

import Eclogues.API (JobError (..))
import Eclogues.Scheduling.Command (ScheduleCommand (..))
import Eclogues.State.Monad (EState)
import qualified Eclogues.State.Monad as ES
import Eclogues.State.Types (AppState, Jobs, jobs)
import Eclogues.TaskSpec ( Name, FailureReason (..), RunErrorReason (..)
                         , TaskSpec, taskName, taskDependsOn
                         , JobState (..), isActiveState, isTerminationState
                                        , isExpectedTransition, isOnScheduler
                         , JobStatus (JobStatus), jobState, jobUuid
                         , QueueStage (LocalQueue) )

import Control.Applicative ((*>), pure)
import Control.Lens ((^.), view)
import Control.Monad (when, void)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, Except, throwE)
import Control.Monad.Trans.Writer.Lazy (WriterT, tell, execWriterT)
import Data.HashMap.Lazy (elems, traverseWithKey)
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (Sum (Sum))
import Data.UUID (UUID)

lift2 :: (MonadTrans m1, MonadTrans m2, Monad m, Monad (m1 m)) => m a -> m2 (m1 m) a
lift2 = lift . lift

createJob :: UUID -> TaskSpec -> ExceptT JobError EState ()
createJob uuid spec = do
    let name = spec ^. taskName
        deps = spec ^. taskDependsOn
    existing <- lift $ ES.getJob name
    when (isJust existing) $ throwE JobNameUsed
    Sum activeDepCount <- execWriterT $ mapM_ (checkDep name) deps
    let jstate = if activeDepCount == 0
            then Queued LocalQueue
            else Waiting activeDepCount
    lift . ES.insertJob $ JobStatus spec jstate uuid
    if jstate == Queued LocalQueue
        then lift . ES.schedule $ QueueJob spec uuid
        else pure ()
    where
        checkDep :: Name -> Name -> WriterT (Sum Integer) (ExceptT JobError EState) ()
        checkDep name depName = lift2 (ES.getJob depName) >>= \case
            Just ds -> case ds ^. jobState of
                Finished            -> lift2 $ ES.addRevDep depName name
                s | isActiveState s -> tell 1 *> lift2 (ES.addRevDep depName name)
                  | otherwise       -> lift . throwE $ JobCannotHaveFailed depName
            Nothing -> lift . throwE $ JobMustExist depName

existingJob :: Name -> ExceptT JobError EState JobStatus
existingJob name = lift (ES.getJob name) >>= \case
    Just js -> pure js
    Nothing -> throwE NoSuchJob

killJob :: Name -> ExceptT JobError EState ()
killJob name = existingJob name >>= \js ->
    if isTerminationState (js ^. jobState)
        then throwE $ JobMustBeTerminated False
        else lift . ES.schedule $ KillJob name (js ^. jobUuid)

deleteJob :: Name -> ExceptT JobError EState ()
deleteJob name = existingJob name >>= \js -> do
    when (isActiveState $ js ^. jobState) . throwE $ JobMustBeTerminated True
    dependents <- lift $ ES.getDependents name
    when (not $ null dependents) . throwE $ OutstandingDependants dependents
    lift $ ES.deleteJob name
    lift . ES.schedule $ CleanupJob name (js ^. jobUuid)

-- Only check on active jobs; terminated jobs shouldn't change status
-- Also Waiting jobs aren't in Aurora so filter out those
activeJobs :: AppState -> Jobs
activeJobs = HashMap.filter (isNonWaitingActiveState . view jobState) . view jobs where
    isNonWaitingActiveState :: JobState -> Bool
    isNonWaitingActiveState (Waiting _) = False
    isNonWaitingActiveState s           = isActiveState s

updateJobs :: Jobs -> [(Name, JobState)] -> EState ()
updateJobs activeStatuses gotStates = void $ traverseWithKey transition activeStatuses where
    transition :: Name -> JobStatus -> EState ()
    transition name pst =
        let oldState = pst ^. jobState
            gotState = fromMaybe whenMissing $ lookup name gotStates
            whenMissing = if isOnScheduler oldState
                then RunError SchedulerLost
                else oldState
        in when (gotState /= oldState) $ do
            let newState = checkTransition oldState gotState
            ES.setJobState name newState
            handleDeps pst newState
    checkTransition :: JobState -> JobState -> JobState
    checkTransition old new = if isExpectedTransition old new then new else RunError BadSchedulerTransition
    handleDeps :: JobStatus -> JobState -> EState ()
    handleDeps pst newState
        | isTerminationState newState = do
            let name = pst ^. taskName
            rdepNames <- ES.getDependents name
            -- Remove this job from the rev deps of its dependencies
            mapM_ (flip ES.removeRevDep name) $ pst ^. taskDependsOn
            case newState of
                Finished -> mapM_ triggerDep rdepNames
                _        -> mapM_ (flip ES.setJobState . Failed $ DependencyFailed name) rdepNames
        | otherwise                  = pure ()
    triggerDep :: Name -> EState ()
    triggerDep rdepName = ES.getJob rdepName >>= \case
        Just (JobStatus spec (Waiting 1) uuid) -> do
            ES.schedule $ QueueJob spec uuid
            ES.setJobState rdepName $ Queued LocalQueue
        Just (JobStatus _    (Waiting n) _   ) ->
            ES.setJobState rdepName $ Waiting $ n - 1
        _         -> pure ()

getJobs :: AppState -> [JobStatus]
getJobs = elems . view jobs

getJob :: Name -> AppState -> Except JobError JobStatus
getJob name state = case HashMap.lookup name (state ^. jobs) of
    Nothing -> throwE NoSuchJob
    Just js -> pure js
