{-# LANGUAGE OverloadedStrings #-}

module AuroraAPI (Client, Result, thriftClient, getJobs, acquireLock, releaseLock, createJob) where

import Api_Types ( Response (Response, response_responseCode, response_result)
                 , JobConfiguration, getJobsResult_configs, result_getJobsResult
                 , SessionKey (..), LockValidation (UNCHECKED)
                 , Lock, result_acquireLockResult, acquireLockResult_lock, default_Lock )
import qualified Api_Types
import Api_Types2 (ResponseCode (OK))
import qualified AuroraSchedulerManager as A
import qualified AuroraSchedulerManager_Client as AClient
import qualified ReadOnlyScheduler_Client as ROClient

import AuroraConfig (auroraJobConfig, lockKey)
import TaskSpec (TaskSpec (..), Name)

import Control.Applicative ((<$>), pure)
import qualified Data.HashSet as HashSet
import qualified Data.Text.Lazy as L
import Network.URI (URI)
import Thrift.Protocol.JSON (JSONProtocol (..))
import Thrift.Transport.HttpClient (HttpClient, openHttpClient)

type Client = (JSONProtocol HttpClient, JSONProtocol HttpClient)
type Result a = Either Response a

onlyOK :: Response -> Result Response
onlyOK res = case response_responseCode res of
    Api_Types2.OK -> Right res
    otherwise     -> Left res

onlyRes :: Response -> Result Api_Types.Result
onlyRes resp = case response_result <$> onlyOK resp of
    Right (Just res) -> Right res
    _                -> Left resp

getJobs :: Client -> IO (Result (HashSet.HashSet JobConfiguration))
getJobs client = (fmap (getJobsResult_configs . result_getJobsResult) . onlyRes) <$> ROClient.getJobs client ""

unauthenticated :: SessionKey
unauthenticated = SessionKey (Just "UNAUTHENTICATED") (Just "UNAUTHENTICATED")

acquireLock :: Client -> Name -> IO (Result Lock)
acquireLock client name = fmap (acquireLockResult_lock . result_acquireLockResult) . onlyRes <$> AClient.acquireLock client (lockKey name) unauthenticated

releaseLock :: Client -> Lock -> IO (Result ())
releaseLock client lock = fmap (const ()) . onlyOK <$> AClient.releaseLock client lock UNCHECKED unauthenticated

createJob :: Client -> TaskSpec -> IO (Result Api_Types.Result)
createJob client spec = onlyRes <$> AClient.createJob client (auroraJobConfig spec) Nothing unauthenticated

thriftClient :: URI -> IO Client
thriftClient uri = do
    transport <- openHttpClient uri
    let proto = JSONProtocol transport
    pure (proto, proto)
