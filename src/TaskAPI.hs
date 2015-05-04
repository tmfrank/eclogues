{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module TaskAPI ( AppState (..), newAppState
               , JobStatus (..)
               , JobError (..), createJob, updateJobs, getJob, getJobs, killJob, deleteJob )
               where

import Api_Types (Response)

import qualified AuroraAPI as A
import AuroraConfig (getJobName, getJobState)
import TaskSpec ( TaskSpec (taskName, taskCommand), Name, FailureReason (..)
                , JobState (..), isActiveState )

import Prelude hiding (writeFile)

import Control.Applicative ((<$>), pure)
import Control.Arrow ((&&&))
import Control.Concurrent.STM (TVar, newTVar, readTVar, modifyTVar, atomically)
import Control.Exception (IOException, try, tryJust)
import Control.Monad (when, guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), withExceptT, throwE, runExceptT)
import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Char (toLower)
import Data.HashMap.Lazy (HashMap, empty, insert, delete, keys, elems, union)
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

data JobError = UnknownResponse Response
              | NoSuchJob
              | InvalidOperation String
                deriving (Show)

jobDir :: AppState -> Name -> FilePath
jobDir state n = jobsDir state ++ "/" ++ L.unpack n

createJob :: AppState -> TaskSpec -> ExceptT JobError IO ()
createJob state spec = do
    let name = taskName spec
    let dir = jobDir state name
    lift $ createDirectoryIfMissing False dir
    lift $ createDirectoryIfMissing False $ dir ++ "/workspace"
    lift $ writeFile (dir ++ "/spec.json") (encode spec)
    let subspec = spec { taskCommand = "aurora-rest-subexecutor " <> name }
    client <- lift . A.thriftClient $ auroraURI state
    withExceptT UnknownResponse $ A.createJob client subspec
    -- TODO: check name is used within atomically
    lift . atomically . modifyTVar (jobs state) $ insert name (JobStatus spec Waiting)

killJob :: AppState -> Name -> ExceptT JobError IO ()
killJob state name = do
    _ <- getJob' state name
    client <- lift . A.thriftClient $ auroraURI state
    withExceptT UnknownResponse $ A.killTasks client [name]

deleteJob :: AppState -> Name -> ExceptT JobError IO ()
deleteJob state name = do
    js <- getJob' state name
    when (isActiveState $ jobState js) $ throwE $ InvalidOperation "Cannot delete running job"
    _ <- lift $ tryJust (guard . isDoesNotExistError) $ removeDirectoryRecursive $ jobDir state name
    -- TODO: possible race if delete and insert happens around here
    lift . atomically . modifyTVar (jobs state) $ delete name

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
getJob st name = HashMap.lookup name <$> atomically (readTVar $ jobs st)

getJob' :: AppState -> Name -> ExceptT JobError IO JobStatus
getJob' st jid = lift (getJob st jid) >>= \case
    Nothing -> throwE NoSuchJob
    Just js -> pure js
