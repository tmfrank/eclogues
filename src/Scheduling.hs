{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Scheduling where

import Prelude hiding (writeFile)

import AppConfig (AppConfig (..))
import qualified AuroraAPI as A
import AuroraConfig (getJobName, getJobState)
import TaskSpec ( TaskSpec (..), JobState (..), Name
                , FailureReason (NonZeroExitCode, TimeExceeded), RunResult (..))

import Control.Applicative ((<$>), pure)
import Control.Arrow ((&&&))
import Control.Exception (IOException, try, tryJust)
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson (encode)
import Data.ByteString.Lazy (writeFile)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as L
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.IO.Error (isDoesNotExistError)
import Text.Read.HT (maybeRead)

data ScheduleCommand r where
    QueueJob :: TaskSpec -> ScheduleCommand ()
    KillJob :: Name -> ScheduleCommand ()
    CleanupJob :: Name -> ScheduleCommand ()
    GetStatuses :: [Name] -> ScheduleCommand [(Name, JobState)]

jobDir :: AppConfig -> Name -> FilePath
jobDir conf n = jobsDir conf ++ "/" ++ L.unpack n

runScheduleCommand :: AppConfig -> ScheduleCommand r -> ExceptT A.UnexpectedResponse IO r
runScheduleCommand conf (QueueJob spec) = do
    let dir = jobDir conf $ taskName spec
        subspec = spec { taskCommand = "eclogues-subexecutor " <> taskName spec }
    lift $ do
        createDirectoryIfMissing False dir
        createDirectoryIfMissing False $ dir ++ "/workspace"
        createDirectoryIfMissing False $ dir ++ "/output"
        writeFile (dir ++ "/spec.json") (encode spec)
    client <- lift $ A.thriftClient $ auroraURI conf
    A.createJob client subspec
runScheduleCommand conf (KillJob name) = do
    client <- lift $ A.thriftClient $ auroraURI conf
    A.killTasks client [name]
runScheduleCommand conf (CleanupJob name) = lift . void $
    tryJust (guard . isDoesNotExistError) . removeDirectoryRecursive $ jobDir conf name
runScheduleCommand conf (GetStatuses names) = do
    client <- lift $ A.thriftClient $ auroraURI conf
    auroraTasks <- A.getTasksWithoutConfigs client names
    let newUncheckedStates = (getJobName &&& getJobState) <$> auroraTasks
    lift $ mapM checkFinState newUncheckedStates
    where
        checkFinState :: (Name, JobState) -> IO (Name, JobState)
        checkFinState (n, Finished) = do
            exitCodeStrM <- try $ readFile (jobDir conf n ++ "/runresult") :: IO (Either IOException String)
            case exitCodeStrM of
                Left  _ -> pure (n, RunError)
                Right a -> pure . (n,) . fromMaybe RunError $ checkRunResult <$> maybeRead a
        checkFinState e = pure e
        checkRunResult :: RunResult -> JobState
        checkRunResult = \case
            Ended ExitSuccess     -> Finished
            Ended (ExitFailure c) -> Failed (NonZeroExitCode c)
            Overtime              -> Failed TimeExceeded
