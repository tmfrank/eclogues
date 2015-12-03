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

module Eclogues.Scheduling.Command (
      ScheduleCommand (..), ScheduleConf (..), AuroraURI
    , runScheduleCommand, getSchedulerStatuses, schedulerJobUI
    -- * Exports for testing
    , lookupNewStages
    ) where

import Prelude hiding (writeFile, readFile)

import qualified Eclogues.Scheduling.AuroraAPI as A
import Eclogues.Scheduling.AuroraConfig (
    Role, ScheduledTask, getJobName, getJobStage)
import Eclogues.Job (
      Stage (..), QueueStage (..)
    , FailureReason (..), RunErrorReason (..))
import qualified Eclogues.Job as Job
import Eclogues.Paths (runResult, specFile)
import Eclogues.Util (RunResult (..), dirName)

import Control.Arrow ((&&&))
import Control.Exception (IOException, try, tryJust)
import Control.Lens ((^.), (&), (.~), view)
import Control.Monad ((<=<), guard, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson (encode, eitherDecode')
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as L
import Data.UUID (UUID)
import Network.URI (URI (uriPath))
import Path ((</>), mkRelDir)
import Path.IO (Dir, readFile, writeFile, createDirectoryIfMissing, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.IO.Error (isDoesNotExistError)

-- | Tell the scheduler to do something.
data ScheduleCommand = QueueJob   Job.Spec UUID
                     | KillJob    Job.Name UUID
                     | CleanupJob Job.Name UUID

$(deriveJSON defaultOptions ''ScheduleCommand)

type AuroraURI = URI
data ScheduleConf = ScheduleConf { jobsDir :: Dir, auroraRole :: Role, auroraURI :: AuroraURI }

jobDir :: ScheduleConf -> Job.Name -> Dir
jobDir conf n = jobsDir conf </> dirName n

runScheduleCommand :: ScheduleConf -> ScheduleCommand -> ExceptT A.UnexpectedResponse IO ()
runScheduleCommand conf (QueueJob spec uuid) = do
    let dir = jobDir conf $ spec ^. Job.name
        subspec = spec & Job.command .~ "eclogues-subexecutor " <> Job.nameText (spec ^. Job.name)
                       & Job.name    .~ Job.uuidName uuid
    lift $ do
        createDirectoryIfMissing False dir
        createDirectoryIfMissing False $ dir </> $(mkRelDir "output")
        writeFile (dir </> specFile) (encode spec)
    client <- lift $ A.thriftClient $ auroraURI conf
    A.createJob client (auroraRole conf) subspec
runScheduleCommand conf (KillJob _name uuid) = do
    client <- lift $ A.thriftClient $ auroraURI conf
    A.killTasks client (auroraRole conf) [Job.uuidName uuid]
runScheduleCommand conf (CleanupJob name _uuid) = lift . void $
    tryJust (guard . isDoesNotExistError) . removeDirectoryRecursive $ jobDir conf name

getSchedulerStatuses :: ScheduleConf -> [Job.Status] -> ExceptT A.UnexpectedResponse IO [(Job.Name, Job.Stage)]
getSchedulerStatuses conf jss = do
    client <- lift $ A.thriftClient $ auroraURI conf
    auroraTasks <- A.getTasksWithoutConfigs client (auroraRole conf) (map fst uuids)
    let newUncheckedStages = lookupNewStages uuids auroraTasks
    lift $ mapM checkFinStage newUncheckedStages
    where
        checkFinStage :: (Job.Name, Job.Stage) -> IO (Job.Name, Job.Stage)
        checkFinStage (n, Finished) = do
            exitCodeStrM <- try $ readFile (jobDir conf n </> runResult) :: IO (Either IOException ByteString)
            case exitCodeStrM of
                Left  _ -> pure (n, RunError SubexecutorFailure)
                Right a -> pure . (n,) . checkRunResult $ eitherDecode' a
        checkFinStage e = pure e
        checkRunResult :: Either e RunResult -> Job.Stage
        checkRunResult = either (const $ RunError SubexecutorFailure) $ \case
            Ended ExitSuccess     -> Finished
            Ended (ExitFailure c) -> Failed (NonZeroExitCode c)
            Overtime              -> Failed TimeExceeded
        uuids = map (Job.uuidName . view Job.uuid &&& view Job.name) jss

lookupNewStages ::
       [(Job.Name, Job.Name)]  -- ^ Mapping of job UUID to name
    -> [ScheduledTask]         -- ^ Aurora responses
    -> [(Job.Name, Job.Stage)] -- ^ Mapping of job name to new stage
lookupNewStages uuids auroraTasks = HM.toList $ foldl' go HM.empty auroraTasks
  where
    go acc task = case extractStage task of
        Nothing      -> acc
        Just (n, st) -> HM.insertWith resolv n st acc
    resolv (Queued SchedulerQueue) x = x
    resolv x (Queued SchedulerQueue) = x
    resolv new _old                  = new
    extractStage at = (, getJobStage at) <$> aUuidToName at
    aUuidToName = flip lookup uuids <=< Job.mkName . L.toStrict . getJobName

schedulerJobUI :: String -> URI -> UUID -> URI
schedulerJobUI user uri uuid = uri { uriPath = "/scheduler/" ++ user ++ "/devel/" ++ show uuid }
