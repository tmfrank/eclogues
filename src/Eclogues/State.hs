{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Eclogues.State (
      AppState (..), newAppState
    , JobStatus (..)
    , JobError (..), createJob, updateJobs, getJob, getJobs, activeJobs, killJob, deleteJob )
    where

import Eclogues.API (JobError (..))
import Eclogues.Scheduling.Command (ScheduleCommand (..))
import Eclogues.State.Monad (EState, insertJob, schedule, addRevDep, getDependents)
import qualified Eclogues.State.Monad as ES
import Eclogues.State.Types (AppState (..), Jobs, newAppState)
import Eclogues.TaskSpec ( TaskSpec (..), Name, FailureReason (..), RunErrorReason (..)
                         , JobState (..), isActiveState, isTerminationState
                                        , isExpectedTransition, isOnScheduler
                         , JobStatus (..), QueueStage (LocalQueue) )

import Control.Applicative ((<$>), (*>), pure)
import Control.Monad (when, foldM)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, Except, throwE)
import Control.Monad.Trans.Writer.Lazy (Writer, WriterT, runWriter, tell, execWriterT)
import Data.HashMap.Lazy (HashMap, adjust, elems, union, traverseWithKey)
import qualified Data.HashMap.Lazy as HashMap
import Data.List (foldl')
import qualified Data.List as List
import Data.Maybe (fromMaybe, catMaybes, isJust)
import Data.Monoid (Sum (Sum))


lift2 :: (MonadTrans m1, MonadTrans m2, Monad m, Monad (m1 m)) => m a -> m2 (m1 m) a
lift2 = lift . lift

jobName :: JobStatus -> Name
jobName = taskName . jobSpec

createJob :: TaskSpec -> ExceptT JobError EState ()
createJob spec = do
    let name = taskName spec
        deps = taskDependsOn spec
    existing <- lift $ ES.getJob name
    when (isJust existing) $ throwE JobNameUsed
    Sum activeDepCount <- execWriterT $ mapM_ (checkDep name) deps
    let jstate = if activeDepCount == 0
            then Queued LocalQueue
            else Waiting activeDepCount
    lift $ insertJob spec jstate
    if jstate == Queued LocalQueue
        then lift . schedule $ QueueJob spec
        else pure ()
    where
        checkDep :: Name -> Name -> WriterT (Sum Integer) (ExceptT JobError EState) ()
        checkDep name depName = lift2 (ES.getJob depName) >>= \case
            Just ds -> case jobState ds of
                Finished            -> lift2 $ addRevDep depName name
                s | isActiveState s -> tell 1 *> lift2 (addRevDep depName name)
                  | otherwise       -> lift . throwE $ JobCannotHaveFailed depName
            Nothing -> lift . throwE $ JobMustExist depName

existingJob :: Name -> ExceptT JobError EState JobStatus
existingJob name = lift (ES.getJob name) >>= \case
    Just js -> pure js
    Nothing -> throwE NoSuchJob

killJob :: Name -> ExceptT JobError EState ()
killJob name = existingJob name >>= \js ->
    if isTerminationState (jobState js)
        then throwE $ JobMustBeTerminated False
        else lift . schedule $ KillJob name

deleteJob :: Name -> ExceptT JobError EState ()
deleteJob name = existingJob name >>= \js -> do
    when (isActiveState $ jobState js) . throwE $ JobMustBeTerminated True
    dependents <- lift $ getDependents name
    when (not $ null dependents) . throwE $ OutstandingDependants dependents
    lift $ ES.deleteJob name
    lift . schedule $ CleanupJob name

-- Only check on active jobs; terminated jobs shouldn't change status
-- Also Waiting jobs aren't in Aurora so filter out those
activeJobs :: AppState -> Jobs
activeJobs state = HashMap.filter (isNonWaitingActiveState . jobState) (jobs state) where
    isNonWaitingActiveState :: JobState -> Bool
    isNonWaitingActiveState (Waiting _) = False
    isNonWaitingActiveState s           = isActiveState s

data StateTransition = Transition JobStatus JobState

updateJobs :: AppState -> Jobs -> [(Name, JobState)] -> (AppState, [ScheduleCommand])
updateJobs state activeStatuses newStates = (AppState statuses'' newRdeps, commands) where
    (activeStatuses', transitions) = runWriter $ traverseWithKey transition activeStatuses
    -- For union, first map has priority
    statuses' = activeStatuses' `union` (jobs state)
    ((statuses'', newRdeps), commands) = runWriter $ foldM handleDeps (statuses', (revDeps state)) transitions

    transition :: Name -> JobStatus -> Writer [StateTransition] JobStatus
    transition name pst = do
        let newState = fromMaybe whenMissing $ lookup name newStates
            whenMissing = if isOnScheduler oldState
                then RunError SchedulerLost
                else oldState
            oldState = jobState pst
            chng st  = tell [Transition pst st] *> pure pst{ jobState = newState }
        if newState == oldState
            then pure pst
            -- TODO: can this ever be False? we don't query non-active jobs.
            else if isExpectedTransition oldState newState
                then chng newState
                else chng $ RunError BadSchedulerTransition
    handleDeps :: (Jobs, HashMap Name [Name]) -> StateTransition -> Writer [ScheduleCommand] (Jobs, HashMap Name [Name])
    handleDeps (jss, allRdeps) (Transition pst newState) | isTerminationState newState = do
        let name = jobName pst
            rdepNames = fromMaybe [] $ HashMap.lookup name allRdeps
            rdeps = catMaybes $ flip HashMap.lookup jss <$> rdepNames
            depNames = taskDependsOn $ jobSpec pst
            -- Remove this job from the rev deps of its dependencies
            allRdeps' = foldl' (removeRdep name) allRdeps depNames
        jss' <- case newState of
            Finished -> foldM triggerDep jss rdeps
            _        -> return $ foldl' (cancelDep name) jss rdepNames
        return (jss', allRdeps')
    handleDeps jss _ = return jss
    removeRdep :: Name -> HashMap Name [Name] -> Name -> HashMap Name [Name]
    removeRdep name allRdeps depName = case rdeps' of
        [] -> HashMap.delete depName allRdeps
        l  -> HashMap.insert depName l allRdeps
        where
            rdeps' = fromMaybe [] $ List.delete name <$> HashMap.lookup depName allRdeps
    cancelDep :: Name -> Jobs -> Name -> Jobs
    cancelDep name jss depName = adjust (setJobState . Failed $ DependencyFailed name) depName jss
    triggerDep :: Jobs -> JobStatus -> Writer [ScheduleCommand] Jobs
    triggerDep jss depSt
        | Waiting 1 <- jobState depSt = do
            tell [QueueJob $ jobSpec depSt]
            return $ adjust (setJobState $ Queued LocalQueue) (jobName depSt) jss
        | Waiting n <- jobState depSt =
            return $ adjust (setJobState $ Waiting $ n - 1) (jobName depSt) jss
        | otherwise                   = return jss
    setJobState :: JobState -> JobStatus -> JobStatus
    setJobState st js = js { jobState = st }

getJobs :: AppState -> [JobStatus]
getJobs = elems . jobs

getJob :: Name -> AppState -> Except JobError JobStatus
getJob name state = case HashMap.lookup name (jobs state) of
    Nothing -> throwE NoSuchJob
    Just js -> pure js
