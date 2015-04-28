{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TaskAPI ( AppState (..), newAppState
               , JobStatus (..), JobState (..), KillReason (..)
               , JobError (..), createJob, updateJobs, getJob, getJobs, killJob )
               where

import Api_Types (Response)

import qualified AuroraAPI as A
import AuroraConfig (JobState (..), KillReason (..), jobName, getJobState)
import TaskSpec (TaskSpec (name, command), Name)

import Prelude hiding (writeFile)

import Control.Applicative ((<$>), pure)
import Control.Arrow ((&&&))
import Control.Concurrent.STM (TVar, newTVar, readTVar, writeTVar, atomically)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), withExceptT, throwE)
import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.List (find, unionBy)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as L
import Data.ByteString.Lazy (writeFile)
import Network.URI (URI)
import System.Directory (createDirectoryIfMissing)

data JobStatus = JobStatus { jobSpec  :: TaskSpec
                           , jobState :: JobState }
                           deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''JobStatus)

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

createJob :: AppState -> TaskSpec -> ExceptT JobError IO ()
createJob state spec = do
    let dir = (jobsDir state) ++ "/" ++ (L.unpack $ name spec)
    lift $ createDirectoryIfMissing False dir
    lift $ createDirectoryIfMissing False $ dir ++ "/workspace"
    lift $ writeFile (dir ++ "/spec.json") (encode spec)
    let subspec = spec { command = "aurora-subexecutor " <> name spec }
    client <- lift . A.thriftClient $ auroraURI state
    withExceptT UnknownResponse . ExceptT $ A.createJob client subspec
    lift . atomically $ do
        let jsv = jobs state
        js <- readTVar jsv
        writeTVar jsv $ (JobStatus spec Waiting):js

killJob :: AppState -> Name -> ExceptT JobError IO ()
killJob state jid = lift (getJob state jid) >>= \case
    Nothing -> throwE NoSuchJob
    Just _  -> do
        client <- lift . A.thriftClient $ auroraURI state
        withExceptT UnknownResponse . ExceptT $ A.killTasks client [jid]

updateJobs :: AppState -> IO ()
updateJobs state = do
    client <- A.thriftClient $ auroraURI state
    prevStatuses <- atomically $ readTVar $ jobs state
    -- TODO: don't pattern match on Right
    Right auroraTasks <- A.getTasksWithoutConfigs client $ name . jobSpec <$> prevStatuses
    let
        newStates :: [(Name, JobState)]
        newStates = (jobName &&& getJobState) <$> auroraTasks
        updatedStatuses :: [JobStatus]
        updatedStatuses = flip fmap prevStatuses $ \pst ->
            case lookup (name $ jobSpec pst) newStates of
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
        jobNameEq s1 s2 = name (jobSpec s1) == name (jobSpec s2)

getJobs :: AppState -> IO [JobStatus]
getJobs = atomically . readTVar . jobs

getJob :: AppState -> Name -> IO (Maybe JobStatus)
getJob st jid = find ((== jid) . name . jobSpec) <$> atomically (readTVar $ jobs st)
