{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Client API for Apache Aurora.
-}

module Eclogues.Scheduling.AuroraAPI (
      Client, Result, JobConfiguration, UnexpectedResponse (..)
    , thriftClient, acquireLock, releaseLock, createJob
    , getJobs, getTasksWithoutConfigs, killTasks ) where

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

import qualified Eclogues.Job as Job
import Eclogues.Scheduling.AuroraConfig (Role, auroraJobConfig, lockKey, defaultJobKey)

import Control.Exception (Exception)
import Control.Monad (void)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Foldable (toList)
import qualified Data.HashSet as HashSet
import qualified Data.Text.Lazy as L
import Data.Typeable (Typeable)
import Network.URI (URI)
import Thrift.Protocol.JSON (JSONProtocol (..))
import Thrift.Transport.HttpClient (HttpClient, openHttpClient)

data UnexpectedResponse = UnexpectedResponse Response deriving (Show, Typeable)
instance Exception UnexpectedResponse

type Client = (JSONProtocol HttpClient, JSONProtocol HttpClient)
type Result a = ExceptT UnexpectedResponse IO a

onlyOK :: Response -> Either UnexpectedResponse Response
onlyOK res = case response_responseCode res of
    Api_Types2.OK -> Right res
    _             -> Left $ UnexpectedResponse res

onlyRes :: Response -> Either UnexpectedResponse Api_Types.Result
onlyRes resp = case response_result <$> onlyOK resp of
    Right (Just res) -> Right res
    _                -> Left $ UnexpectedResponse resp

-- TODO: output a list?
getJobs :: Client -> Result (HashSet.HashSet JobConfiguration)
getJobs client = do
    res <- ExceptT $ onlyRes <$> ROClient.getJobs client ""
    pure . getJobsResult_configs $ result_getJobsResult res

taskQuery :: Role -> [Job.Name] -> TaskQuery
taskQuery role names = default_TaskQuery { taskQuery_jobKeys = Just . HashSet.fromList $ key <$> names }
  where
    key = defaultJobKey role . L.fromStrict . Job.nameText

getTasksWithoutConfigs :: Client -> Role -> [Job.Name] -> Result [ScheduledTask]
getTasksWithoutConfigs client role names = do
    res <- ExceptT $ onlyRes <$> ROClient.getTasksWithoutConfigs client q
    pure $ tasks res
    where
        q = taskQuery role names
        tasks = toList . scheduleStatusResult_tasks . result_scheduleStatusResult

unauthenticated :: SessionKey
unauthenticated = SessionKey (Just "UNAUTHENTICATED") (Just "UNAUTHENTICATED")

acquireLock :: Client -> Role -> Job.Name -> Result Lock
acquireLock client role name = do
    let name' = L.fromStrict . Job.nameText $ name
    res <- ExceptT $ onlyRes <$> AClient.acquireLock client (lockKey role name') unauthenticated
    pure . acquireLockResult_lock $ result_acquireLockResult res

releaseLock :: Client -> Lock -> Result ()
releaseLock client lock = void . ExceptT $ onlyOK <$> AClient.releaseLock client lock UNCHECKED unauthenticated

createJob :: Client -> Role -> Job.Spec -> Result ()
createJob client role spec = void . ExceptT $ onlyOK <$> AClient.createJob client (auroraJobConfig role spec) Nothing unauthenticated

killTasks :: Client -> Role -> [Job.Name] -> Result ()
killTasks client role names = void . ExceptT $ onlyOK <$> AClient.killTasks client (taskQuery role names) Nothing unauthenticated

thriftClient :: URI -> IO Client
thriftClient uri = do
    transport <- openHttpClient uri
    let proto = JSONProtocol transport
    pure (proto, proto)
