{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Communication with the remote scheduler.
-}

module Eclogues.Scheduling.Command ( ScheduleCommand (..), ScheduleConf (..), AuroraURI
                                   , runScheduleCommand, getSchedulerStatuses, schedulerJobUI ) where

import Prelude hiding (writeFile)

import qualified Eclogues.Scheduling.AuroraAPI as A
import Eclogues.Scheduling.AuroraConfig (Role, getJobName, getJobState)
import Eclogues.JobSpec (
      JobSpec, JobState (..), JobStatus, Name
    , FailureReason (..), RunErrorReason (..), RunResult (..))
import qualified Eclogues.JobSpec as Job

import Control.Arrow ((&&&))
import Control.Exception (IOException, try, tryJust)
import Control.Lens ((^.), (&), (.~), view)
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString.Lazy (writeFile)
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as L
import Data.UUID (UUID)
import Network.URI (URI (uriPath))
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.IO.Error (isDoesNotExistError)
import Text.Read.HT (maybeRead)

-- | Tell the scheduler to do something.
data ScheduleCommand = QueueJob JobSpec UUID
                     | KillJob Name UUID
                     | CleanupJob Name UUID

$(deriveJSON defaultOptions ''ScheduleCommand)

type AuroraURI = URI
data ScheduleConf = ScheduleConf { jobsDir :: FilePath, auroraRole :: Role, auroraURI :: AuroraURI }

jobDir :: ScheduleConf -> Name -> FilePath
jobDir conf n = jobsDir conf ++ "/" ++ L.unpack n

runScheduleCommand :: ScheduleConf -> ScheduleCommand -> ExceptT A.UnexpectedResponse IO ()
runScheduleCommand conf (QueueJob spec uuid) = do
    let dir = jobDir conf $ spec ^. Job.name
        subspec = spec & Job.command .~ "eclogues-subexecutor " <> spec ^. Job.name
                       & Job.name    .~ L.pack (show uuid)
    lift $ do
        createDirectoryIfMissing False dir
        createDirectoryIfMissing False $ dir ++ "/workspace"
        createDirectoryIfMissing False $ dir ++ "/output"
        writeFile (dir ++ "/spec.json") (encode spec)
    client <- lift $ A.thriftClient $ auroraURI conf
    A.createJob client (auroraRole conf) subspec
runScheduleCommand conf (KillJob _name uuid) = do
    client <- lift $ A.thriftClient $ auroraURI conf
    A.killTasks client (auroraRole conf) [L.pack (show uuid)]
runScheduleCommand conf (CleanupJob name _uuid) = lift . void $
    tryJust (guard . isDoesNotExistError) . removeDirectoryRecursive $ jobDir conf name

getSchedulerStatuses :: ScheduleConf -> [JobStatus] -> ExceptT A.UnexpectedResponse IO [(Name, JobState)]
getSchedulerStatuses conf jss = do
    client <- lift $ A.thriftClient $ auroraURI conf
    let uuids = map (L.pack . show . view Job.uuid &&& view Job.name) jss
    auroraTasks <- A.getTasksWithoutConfigs client (auroraRole conf) (map fst uuids)
    let newUncheckedStates = ((fromJust . flip lookup uuids . getJobName) &&& getJobState) <$> auroraTasks
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

schedulerJobUI :: String -> URI -> UUID -> URI
schedulerJobUI user uri uuid = uri { uriPath = "/scheduler/" ++ user ++ "/devel/" ++ show uuid }
