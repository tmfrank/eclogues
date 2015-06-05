{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Eclogues.Scheduling.Command where

import Prelude hiding (writeFile)

import qualified Eclogues.Scheduling.AuroraAPI as A
import Eclogues.Scheduling.AuroraConfig (Role, getJobName, getJobState)
import Eclogues.TaskSpec (
      TaskSpec, JobState (..), Name
    , FailureReason (..), RunErrorReason (..), RunResult (..)
    , taskCommand, taskName)

import Control.Applicative ((<$>), pure)
import Control.Arrow ((&&&))
import Control.Exception (IOException, try, tryJust)
import Control.Lens ((^.), (&), (.~))
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString.Lazy (writeFile)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as L
import Network.URI (URI)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.IO.Error (isDoesNotExistError)
import Text.Read.HT (maybeRead)

-- TODO: split up

data ScheduleCommand = QueueJob TaskSpec
                     | KillJob Name
                     | CleanupJob Name

$(deriveJSON defaultOptions ''ScheduleCommand)

data ScheduleConf = ScheduleConf { jobsDir :: FilePath, auroraRole :: Role, auroraURI :: URI }

jobDir :: ScheduleConf -> Name -> FilePath
jobDir conf n = jobsDir conf ++ "/" ++ L.unpack n

runScheduleCommand :: ScheduleConf -> ScheduleCommand -> ExceptT A.UnexpectedResponse IO ()
runScheduleCommand conf (QueueJob spec) = do
    let dir = jobDir conf $ spec ^. taskName
        subspec = spec & taskCommand .~ "eclogues-subexecutor " <> spec ^. taskName
    lift $ do
        createDirectoryIfMissing False dir
        createDirectoryIfMissing False $ dir ++ "/workspace"
        createDirectoryIfMissing False $ dir ++ "/output"
        writeFile (dir ++ "/spec.json") (encode spec)
    client <- lift $ A.thriftClient $ auroraURI conf
    A.createJob client (auroraRole conf) subspec
runScheduleCommand conf (KillJob name) = do
    client <- lift $ A.thriftClient $ auroraURI conf
    A.killTasks client (auroraRole conf) [name]
runScheduleCommand conf (CleanupJob name) = lift . void $
    tryJust (guard . isDoesNotExistError) . removeDirectoryRecursive $ jobDir conf name

getSchedulerStatuses :: ScheduleConf -> [Name] -> ExceptT A.UnexpectedResponse IO [(Name, JobState)]
getSchedulerStatuses conf names = do
    client <- lift $ A.thriftClient $ auroraURI conf
    auroraTasks <- A.getTasksWithoutConfigs client (auroraRole conf) names
    let newUncheckedStates = (getJobName &&& getJobState) <$> auroraTasks
    lift $ mapM checkFinState newUncheckedStates
    where
        checkFinState :: (Name, JobState) -> IO (Name, JobState)
        checkFinState (n, Finished) = do
            exitCodeStrM <- try $ readFile (jobDir conf n ++ "/runresult") :: IO (Either IOException String)
            case exitCodeStrM of
                Left  _ -> pure (n, RunError SubexecutorFailure)
                Right a -> pure . (n,) . fromMaybe (RunError SubexecutorFailure) $ checkRunResult <$> maybeRead a
        checkFinState e = pure e
        checkRunResult :: RunResult -> JobState
        checkRunResult = \case
            Ended ExitSuccess     -> Finished
            Ended (ExitFailure c) -> Failed (NonZeroExitCode c)
            Overtime              -> Failed TimeExceeded
