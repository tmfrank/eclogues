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
import TaskSpec ( TaskSpec (taskName, taskCommand), Name, FailureReason (NonZeroExitCode)
                , JobState (..), isActiveState, isTerminationState )

import Prelude hiding (writeFile)

import Control.Applicative ((<$>), pure)
import Control.Arrow ((&&&))
import Control.Concurrent.AdvSTM (MonadAdvSTM, AdvSTM, atomically, onCommit)
import Control.Concurrent.AdvSTM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Exception (Exception, IOException, try, tryJust, throwIO)
import Control.Monad (when, guard, void, join)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), withExceptT, throwE, runExceptT, mapExceptT)
import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Char (toLower)
import Data.Either.Combinators (rightToMaybe, mapLeft)
import Data.HashMap.Lazy (HashMap, empty, insert, delete, keys, elems, union, member)
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (fromMaybe)
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
                         , jobs      :: TVar (HashMap Name JobStatus) }

newAppState :: URI -> FilePath -> IO AppState
newAppState auroraURI' jobsDir' = do
    jobs' <- atomically $ newTVar empty
    pure $ AppState jobsDir' auroraURI' jobs'

data JobError = UnexpectedResponse A.UnexpectedResponse
              | JobNameUsed
              | NoSuchJob
              | JobMustBeTerminated Bool
                deriving (Show)

jobDir :: AppState -> Name -> FilePath
jobDir state n = jobsDir state ++ "/" ++ L.unpack n

modifyTVar :: MonadAdvSTM m => TVar a -> (a -> a) -> m ()
modifyTVar var f = readTVar var >>= writeTVar var . f

throwExc :: (Exception e) => ExceptT e IO a -> IO a
throwExc act = runExceptT act >>= \case
    Left e  -> throwIO e
    Right a -> pure a

createJob :: AppState -> TaskSpec -> ExceptT JobError IO ()
createJob state spec = catchUnexpectedResponse . mapExceptT atomically $ do
    let name = taskName spec
    let dir = jobDir state name
    let jobsV = jobs state
    jss <- lift $ readTVar jobsV
    when (member name jss) $ throwE JobNameUsed
    lift . onCommit $ do
        createDirectoryIfMissing False dir
        createDirectoryIfMissing False $ dir ++ "/workspace"
        writeFile (dir ++ "/spec.json") (encode spec)
        let subspec = spec { taskCommand = "aurora-rest-subexecutor " <> name }
        client <- A.thriftClient $ auroraURI state
        throwExc $ A.createJob client subspec
    lift . writeTVar jobsV $ insert name (JobStatus spec Queued) jss
    where
        catchUnexpectedResponse :: ExceptT JobError IO a -> ExceptT JobError IO a
        catchUnexpectedResponse = mapExceptT $ (fmap $ join . mapLeft UnexpectedResponse) . try

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

updateJobs :: AppState -> IO ()
updateJobs state = do
    client <- A.thriftClient $ auroraURI state
    prevStatuses <- atomically $ readTVar $ jobs state
    -- TODO: don't pattern match on Right
    Right auroraTasks <- runExceptT $ A.getTasksWithoutConfigs client $ keys prevStatuses
    let newUncheckedStates = (getJobName &&& getJobState) <$> auroraTasks
    newStates <- mapM checkFinState newUncheckedStates
    let updatedStatuses = flip fmap prevStatuses $ \pst ->
            case lookup (taskName $ jobSpec pst) newStates of
                Just se -> pst { jobState = se }
                Nothing -> pst { jobState = RunError }
    -- For union, first list has priority
    atomically . modifyTVar (jobs state) $ union updatedStatuses
    where
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
