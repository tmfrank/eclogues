{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module TaskAPI ( AppState (..), newAppState
               , JobStatus (..)
               , JobError (..), createJob, updateJobs, getJob, getJobs, killJob, deleteJob )
               where

import qualified AuroraAPI as A
import AuroraConfig (getJobName, getJobState)
import TaskSpec ( TaskSpec (..), Name, FailureReason (NonZeroExitCode, DependencyFailed)
                , JobState (..), isActiveState, isTerminationState )

import Prelude hiding (writeFile)

import Control.Applicative ((<$>), pure)
import Control.Arrow ((&&&))
import Control.Concurrent.AdvSTM (MonadAdvSTM, AdvSTM, atomically, onCommit)
import Control.Concurrent.AdvSTM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Exception (Exception, IOException, try, tryJust, throwIO)
import Control.Monad (when, guard, void, join, foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), withExceptT, throwE, runExceptT, mapExceptT)
import Control.Monad.Trans.Writer.Lazy (Writer, runWriter, tell)
import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Char (toLower)
import Data.Either.Combinators (rightToMaybe, mapLeft)
import Data.HashMap.Lazy ( HashMap, empty, insert, insertWith, delete, adjust
                         , keys, elems, union, member, traverseWithKey )
import qualified Data.HashMap.Lazy as HashMap
import Data.List (foldl')
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as L
import Data.ByteString.Lazy (writeFile)
import Network.URI (URI)
import Text.Read.HT (maybeRead)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.IO.Error (isDoesNotExistError)

data JobStatus = JobStatus { jobSpec  :: TaskSpec
                           , jobState :: JobState }
                           deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 3} ''JobStatus)

data AppState = AppState { jobsDir   :: FilePath
                         , auroraURI :: URI
                         , jobs      :: TVar (HashMap Name JobStatus)
                         , revDeps   :: TVar (HashMap Name [Name]) }

type JSS = HashMap Name JobStatus

newAppState :: URI -> FilePath -> IO AppState
newAppState auroraURI' jobsDir' = atomically $ do
    jobs' <- newTVar empty
    revDeps' <- newTVar empty
    return $ AppState jobsDir' auroraURI' jobs' revDeps'

data JobError = UnexpectedResponse A.UnexpectedResponse
              | JobNameUsed
              | NoSuchJob
              | JobMustExist Name
              | JobCannotHaveFailed Name
              | JobMustBeTerminated Bool
                deriving (Show)

jobDir :: AppState -> Name -> FilePath
jobDir state n = jobsDir state ++ "/" ++ L.unpack n

jobName :: JobStatus -> Name
jobName = taskName . jobSpec

modifyTVar :: MonadAdvSTM m => TVar a -> (a -> a) -> m ()
modifyTVar var f = readTVar var >>= writeTVar var . f

throwExc :: (Exception e) => ExceptT e IO a -> IO a
throwExc act = runExceptT act >>= \case
    Left e  -> throwIO e
    Right a -> pure a

scheduleJob :: URI -> TaskSpec -> ExceptT A.UnexpectedResponse IO ()
scheduleJob uri spec = do
    let subspec = spec { taskCommand = "aurora-rest-subexecutor " <> taskName spec }
    client <- lift $ A.thriftClient uri
    A.createJob client subspec

createJob :: AppState -> TaskSpec -> ExceptT JobError IO ()
createJob state spec = catchUnexpectedResponse . mapExceptT atomically $ do
    let name = taskName spec
    let dir = jobDir state name
    let deps = taskDependsOn spec
    jss <- lift . readTVar $ jobs state
    when (member name jss) $ throwE JobNameUsed
    activeDepCount <- sum <$> mapM (checkDep name jss) deps
    let jstate = if activeDepCount == 0
        then Queued
        else Waiting activeDepCount
    lift . onCommit $ do
        createDirectoryIfMissing False dir
        createDirectoryIfMissing False $ dir ++ "/workspace"
        writeFile (dir ++ "/spec.json") (encode spec)
        when (jstate == Queued) $ throwExc $ scheduleJob (auroraURI state) spec
    lift . writeTVar (jobs state) $ insert name (JobStatus spec jstate) jss
    where
        catchUnexpectedResponse :: ExceptT JobError IO a -> ExceptT JobError IO a
        catchUnexpectedResponse = mapExceptT $ (fmap $ join . mapLeft UnexpectedResponse) . try
        checkDep :: Name -> JSS -> Name -> ExceptT JobError AdvSTM Integer
        checkDep name jss depName
            | Just ds <- HashMap.lookup depName jss = case jobState ds of
                Finished            -> pure 0
                s | isActiveState s -> do
                        lift $ modifyTVar (revDeps state) $ insertWith (++) depName [name]
                        pure 1
                  | otherwise       -> throwE $ JobCannotHaveFailed depName
            | otherwise = throwE $ JobMustExist depName

killJob :: AppState -> Name -> ExceptT JobError IO ()
killJob state name = do
    js <- mapExceptT atomically $ getJob' state name
    when (isTerminationState $ jobState js) . throwE $ JobMustBeTerminated False
    client <- lift . A.thriftClient $ auroraURI state
    withExceptT UnexpectedResponse $ A.killTasks client [name]

deleteJob :: AppState -> Name -> ExceptT JobError IO ()
deleteJob state name = mapExceptT atomically $ do
    js <- getJob' state name
    when (isActiveState $ jobState js) . throwE $ JobMustBeTerminated True
    lift . onCommit . void $ tryJust (guard . isDoesNotExistError) . removeDirectoryRecursive $ jobDir state name
    lift . modifyTVar (jobs state) $ delete name

data StateTransition = Transition Name JobState JobState

updateJobs :: AppState -> IO ()
updateJobs state = do
    client <- A.thriftClient $ auroraURI state
    prevStatuses <- atomically $ readTVar $ jobs state
    -- Only check on active jobs; terminated jobs shouldn't change status
    -- Also Waiting jobs aren't in Aurora so filter out those
    let activeStatuses = HashMap.filter (isNonWaitingActiveState . jobState) prevStatuses
    -- TODO: don't pattern match on Right
    Right auroraTasks <- runExceptT . A.getTasksWithoutConfigs client $ keys activeStatuses
    let newUncheckedStates = (getJobName &&& getJobState) <$> auroraTasks
    newStates <- mapM checkFinState newUncheckedStates
    let (updatedStatuses, transitions) = runWriter $ traverseWithKey (transition newStates) activeStatuses
    atomically $ do
        curStatuses <- readTVar $ jobs state
        rdeps <- readTVar $ revDeps state
        -- For union, first map has priority
        let newStatuses = union updatedStatuses curStatuses
        (newStatuses', rdeps') <- foldM handleDeps (newStatuses, rdeps) transitions
        writeTVar (jobs state) newStatuses'
        writeTVar (revDeps state) rdeps'
    where
        isNonWaitingActiveState :: JobState -> Bool
        isNonWaitingActiveState (Waiting _) = False
        isNonWaitingActiveState s           = isActiveState s
        checkFinState :: (Name, JobState) -> IO (Name, JobState)
        checkFinState (n, Finished) = do
            exitCodeStrM <- try $ readFile (jobDir state n ++ "/exitcode") :: IO (Either IOException String)
            case exitCodeStrM of
                Left  _ -> pure (n, RunError)
                Right a -> pure . (n,) . fromMaybe RunError $ checkExitCode <$> maybeRead a
        checkFinState e = pure e
        checkExitCode :: ExitCode -> JobState
        checkExitCode exitCode = case exitCode of
            ExitSuccess   -> Finished
            ExitFailure c -> Failed (NonZeroExitCode c)
        transition :: [(Name, JobState)] -> Name -> JobStatus -> Writer [StateTransition] JobStatus
        transition newStates name pst = do
            let newState = fromMaybe RunError $ lookup name newStates
            let oldState = jobState pst
            when (newState /= oldState) $ tell $ [Transition name oldState newState]
            pure pst { jobState = newState }
        handleDeps :: (JSS, HashMap Name [Name]) -> StateTransition -> AdvSTM (JSS, HashMap Name [Name])
        handleDeps (jss, rdeps) (Transition name _ newState) | isTerminationState newState = do
            let depNames = fromMaybe [] $ HashMap.lookup name rdeps
            let deps = catMaybes $ flip HashMap.lookup jss <$> depNames
            jss' <- case newState of
                Finished -> foldM triggerDep jss deps
                _        -> return $ foldl' (cancelDep name) jss depNames
            return (jss', delete name rdeps)
        handleDeps jss _ = return jss
        cancelDep :: Name -> JSS -> Name -> JSS
        cancelDep name jss depName = adjust (setJobState . Failed $ DependencyFailed name) depName jss
        triggerDep :: JSS -> JobStatus -> AdvSTM JSS
        triggerDep jss depSt
            | Waiting 1 <- jobState depSt = do
                onCommit . throwExc $ scheduleJob (auroraURI state) (jobSpec depSt)
                return $ adjust (setJobState Queued) (jobName depSt) jss
            | Waiting n <- jobState depSt =
                return $ adjust (setJobState $ Waiting $ n - 1) (jobName depSt) jss
            | otherwise                   = return jss
        setJobState :: JobState -> JobStatus -> JobStatus
        setJobState st js = js { jobState = st }

getJobs :: AppState -> IO [JobStatus]
getJobs = fmap elems . atomically . readTVar . jobs

getJob :: AppState -> Name -> IO (Maybe JobStatus)
getJob state = atomically . fmap rightToMaybe . runExceptT . getJob' state

getJob' :: AppState -> Name -> ExceptT JobError AdvSTM JobStatus
getJob' state name = do
    jss <- lift . readTVar $ jobs state
    case HashMap.lookup name jss of
        Nothing -> throwE NoSuchJob
        Just js -> pure js
