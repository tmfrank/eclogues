{-# LANGUAGE OverloadedStrings #-}

module AuroraAPI ( Client, Result, JobConfiguration
                 , thriftClient, acquireLock, releaseLock, createJob
                 , getJobs, getTasksWithoutConfigs ) where

import Api_Types ( Response (response_responseCode, response_result)
                 , JobConfiguration, getJobsResult_configs, result_getJobsResult
                 , SessionKey (..), LockValidation (UNCHECKED)
                 , Lock, result_acquireLockResult, acquireLockResult_lock
                 , TaskQuery (..), default_TaskQuery
                 , ScheduledTask, result_scheduleStatusResult,scheduleStatusResult_tasks )
import qualified Api_Types
import Api_Types2 (ResponseCode (OK))
import qualified AuroraSchedulerManager_Client as AClient
import qualified ReadOnlyScheduler_Client as ROClient

import AuroraConfig (auroraJobConfig, lockKey, defaultJobKey)
import TaskSpec (TaskSpec, Name)

import Control.Applicative ((<$>), pure)
import Control.Monad (void)
import Data.Foldable (toList)
import qualified Data.HashSet as HashSet
import Network.URI (URI)
import Thrift.Protocol.JSON (JSONProtocol (..))
import Thrift.Transport.HttpClient (HttpClient, openHttpClient)

type Client = (JSONProtocol HttpClient, JSONProtocol HttpClient)
type Result a = Either Response a  -- TODO: ExceptT

onlyOK :: Response -> Result Response
onlyOK res = case response_responseCode res of
    Api_Types2.OK -> Right res
    _             -> Left res

onlyRes :: Response -> Result Api_Types.Result
onlyRes resp = case response_result <$> onlyOK resp of
    Right (Just res) -> Right res
    _                -> Left resp

getJobs :: Client -> IO (Result (HashSet.HashSet JobConfiguration))
getJobs client = (fmap (getJobsResult_configs . result_getJobsResult) . onlyRes) <$> ROClient.getJobs client ""

getTasksWithoutConfigs :: Client -> [Name] -> IO (Result [ScheduledTask])
getTasksWithoutConfigs client names = fmap tasks . onlyRes <$> ROClient.getTasksWithoutConfigs client q where
    q = default_TaskQuery { taskQuery_jobKeys = Just . HashSet.fromList $ defaultJobKey <$> names }
    tasks = toList . scheduleStatusResult_tasks . result_scheduleStatusResult

unauthenticated :: SessionKey
unauthenticated = SessionKey (Just "UNAUTHENTICATED") (Just "UNAUTHENTICATED")

acquireLock :: Client -> Name -> IO (Result Lock)
acquireLock client name = fmap (acquireLockResult_lock . result_acquireLockResult) . onlyRes <$> AClient.acquireLock client (lockKey name) unauthenticated

releaseLock :: Client -> Lock -> IO (Result ())
releaseLock client lock = void . onlyOK <$> AClient.releaseLock client lock UNCHECKED unauthenticated

createJob :: Client -> TaskSpec -> IO (Result ())
createJob client spec = void . onlyOK <$> AClient.createJob client (auroraJobConfig spec) Nothing unauthenticated

thriftClient :: URI -> IO Client
thriftClient uri = do
    transport <- openHttpClient uri
    let proto = JSONProtocol transport
    pure (proto, proto)
