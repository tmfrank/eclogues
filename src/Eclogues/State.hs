{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Eclogues.State (
      AppState (..), newAppState
    , JobStatus (..)
    , JobError (..), createJob, updateJobs, getJob, getJobs, activeJobs, killJob, deleteJob )
    where

import Eclogues.API (JobError (..))
import Eclogues.Scheduling.Command (ScheduleCommand (..))
import Eclogues.TaskSpec ( TaskSpec (..), Name, FailureReason (..), RunErrorReason (..)
                         , JobState (..), isActiveState, isTerminationState
                                        , isExpectedTransition, isOnScheduler
                         , JobStatus (..), QueueStage (LocalQueue) )

import Control.Applicative ((<$>), (*>), pure)
import Control.Monad (when, foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, throwE)
import Control.Monad.Trans.Writer.Lazy (Writer, WriterT, runWriterT, runWriter, tell)
import Data.HashMap.Lazy ( HashMap, empty, insert, insertWith, adjust
                         , elems, union, member, traverseWithKey )
import qualified Data.HashMap.Lazy as HashMap
import Data.List (foldl')
import qualified Data.List as List
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid (Sum (Sum))

data AppState = AppState { jobs      :: HashMap Name JobStatus
                         , revDeps   :: HashMap Name [Name] }

type JSS = HashMap Name JobStatus
type RevDeps = HashMap Name [Name]

newAppState :: AppState
newAppState = AppState empty empty

jobName :: JobStatus -> Name
jobName = taskName . jobSpec

createJob :: TaskSpec -> AppState -> Except JobError (AppState, [ScheduleCommand])
createJob spec state = do
    let name = taskName spec
        deps = taskDependsOn spec
        jss  = jobs state
    when (member name jss) $ throwE JobNameUsed
    (rdeps', Sum activeDepCount) <- runWriterT $ foldM (checkDep name jss) (revDeps state) deps
    let jstate = if activeDepCount == 0
            then Queued LocalQueue
            else Waiting activeDepCount
        jss' = insert name (JobStatus spec jstate) jss
        state' = AppState jss' rdeps'
    if jstate == Queued LocalQueue
        then pure (state', [QueueJob spec])
        else pure (state', [])
    where
        checkDep :: Name -> JSS -> RevDeps -> Name -> WriterT (Sum Integer) (Except JobError) RevDeps
        checkDep name jss rdeps depName
            | Just ds <- HashMap.lookup depName jss = case jobState ds of
                Finished            -> pure $ addRdep name depName rdeps
                s | isActiveState s -> tell 1 *> pure (addRdep name depName rdeps)
                  | otherwise       -> lift . throwE $ JobCannotHaveFailed depName
            | otherwise = lift . throwE $ JobMustExist depName
        addRdep :: Name -> Name -> RevDeps -> RevDeps
        addRdep name depName = insertWith (++) depName [name]

killJob :: Name -> AppState -> Except JobError [ScheduleCommand]
killJob name state = do
    js <- getJob name state
    if isTerminationState (jobState js)
        then throwE $ JobMustBeTerminated False
        else pure [KillJob name]

deleteJob :: Name -> AppState -> Except JobError (AppState, [ScheduleCommand])
deleteJob name state = do
    js <- getJob name state
    when (isActiveState $ jobState js) . throwE $ JobMustBeTerminated True
    fromMaybe (return ()) $ (throwE . OutstandingDependants) <$> HashMap.lookup name (revDeps state)
    let state' = state { jobs = HashMap.delete name (jobs state) }
    pure (state', [CleanupJob name])

-- Only check on active jobs; terminated jobs shouldn't change status
-- Also Waiting jobs aren't in Aurora so filter out those
activeJobs :: AppState -> JSS
activeJobs state = HashMap.filter (isNonWaitingActiveState . jobState) (jobs state) where
    isNonWaitingActiveState :: JobState -> Bool
    isNonWaitingActiveState (Waiting _) = False
    isNonWaitingActiveState s           = isActiveState s

data StateTransition = Transition JobStatus JobState

updateJobs :: AppState -> JSS -> [(Name, JobState)] -> (AppState, [ScheduleCommand])
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
    handleDeps :: (JSS, HashMap Name [Name]) -> StateTransition -> Writer [ScheduleCommand] (JSS, HashMap Name [Name])
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
    cancelDep :: Name -> JSS -> Name -> JSS
    cancelDep name jss depName = adjust (setJobState . Failed $ DependencyFailed name) depName jss
    triggerDep :: JSS -> JobStatus -> Writer [ScheduleCommand] JSS
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
