{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module TaskAPI ( AppState (..), newAppState
               , JobStatus (..)
               , JobError (..), createJob, updateJobs, getJob, getJobs, killJob )
               where

import Api_Types (Response)

import qualified AuroraAPI as A
import AuroraConfig (getJobName, getJobState)
import TaskSpec (TaskSpec (taskName, taskCommand), Name, JobState (..), FailureReason (..))

import Prelude hiding (writeFile)

import Control.Applicative ((<$>), pure)
import Control.Arrow ((&&&))
import Control.Concurrent.STM (TVar, newTVar, readTVar, writeTVar, atomically)
import Control.Exception (IOException, try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), withExceptT, throwE, runExceptT)
import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Char (toLower)
import Data.Function (on)
import Data.List (find, unionBy)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as L
import Data.ByteString.Lazy (writeFile)
import Network.URI (URI)
import Text.Read.HT (maybeRead)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))

data JobStatus = JobStatus { jobSpec  :: TaskSpec
                           , jobState :: JobState }
                           deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 3} ''JobStatus)

-- TODO: store jobs as HashMap
data AppState = AppState { jobsDir   :: FilePath
                         , auroraURI :: URI
                         , jobs      :: TVar [JobStatus] }

newAppState :: URI -> FilePath -> IO AppState
newAppState auroraURI' jobsDir' = do
    jobs' <- atomically $ newTVar []
    pure $ AppState jobsDir' auroraURI' jobs'

data JobError = UnknownResponse Response
              | NoSuchJob
                deriving (Show)

jobName :: JobStatus -> Name
jobName = taskName . jobSpec

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
    lift . atomically $ do
        let jsv = jobs state
        js <- readTVar jsv
        writeTVar jsv $ JobStatus spec Waiting : js

killJob :: AppState -> Name -> ExceptT JobError IO ()
killJob state name = lift (getJob state name) >>= \case
    Nothing -> throwE NoSuchJob
    Just _  -> do
        client <- lift . A.thriftClient $ auroraURI state
        withExceptT UnknownResponse $ A.killTasks client [name]

updateJobs :: AppState -> IO ()
updateJobs state = do
    client <- A.thriftClient $ auroraURI state
    prevStatuses <- atomically $ readTVar $ jobs state
    -- TODO: don't pattern match on Right
    Right auroraTasks <- runExceptT $ A.getTasksWithoutConfigs client $ jobName <$> prevStatuses
    let newUncheckedStates = (getJobName &&& getJobState) <$> auroraTasks
    newStates <- mapM checkFinState newUncheckedStates
    let updatedStatuses = flip fmap prevStatuses $ \pst ->
            case lookup (taskName $ jobSpec pst) newStates of
                Just se -> pst { jobState = se }
                Nothing -> pst { jobState = RunError }
    atomically $ do
        let jobsV = jobs state
        statuses <- readTVar jobsV
        -- For unionBy, first list has priority
        let statuses' = unionBy jobNameEq updatedStatuses statuses
        writeTVar jobsV statuses'
    where
        jobNameEq :: JobStatus -> JobStatus -> Bool
        jobNameEq = (==) `on` jobName
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
getJobs = atomically . readTVar . jobs

getJob :: AppState -> Name -> IO (Maybe JobStatus)
getJob st name = find ((== name) . jobName) <$> atomically (readTVar $ jobs st)
