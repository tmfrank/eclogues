{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Interaction with an Eclogues web service.
-}

module Eclogues.Client (
                       -- * Client
                         EcloguesClient, Result, ecloguesClient
                       -- ** View
                       , getJobs, getJobStatus, getJobState, getHealth, masterHost
                       -- ** Mutate
                       , createJob, deleteJob, setJobState
                       -- * Zookeeper
                       , getEcloguesLeader
                       ) where

import Database.Zookeeper.Election (ZookeeperError, getLeaderInfo)
import Database.Zookeeper.ManagedEvents (ManagedZK)
import Eclogues.API (VAPI, JobError, Health)
import Eclogues.ServantInstances ()
import Eclogues.JobSpec (JobStatus, JobState, Name, JobSpec)

import Control.Monad ((<=<))
import Control.Monad.Trans.Either (runEitherT)
import Control.Monad.Trans.Except (ExceptT (..), withExceptT)
import Data.Aeson (eitherDecode, eitherDecodeStrict)
import Data.Either.Combinators (mapLeft)
import Data.Proxy (Proxy (..))
import Data.Word (Word16)
import Servant.API ((:<|>) (..))
import Servant.Client (BaseUrl (..), Scheme (Http), ServantError (..), client)

type Result a = ExceptT (Either ServantError JobError) IO a

-- | Functions for interacting with an Eclogues master. See 'ecloguesClient'.
data EcloguesClient = EcloguesClient { _getJobs      ::            Result [JobStatus]
                                     , _getJobStatus :: Name    -> Result JobStatus
                                     , _getJobState  :: Name    -> Result JobState
                                     , _setJobState  :: Name    -> JobState -> Result ()
                                     , _deleteJob    :: Name    -> Result ()
                                     , _createJob    :: JobSpec -> Result ()
                                     , _getHealth    ::            Result Health
                                     , _masterHost   :: (String, Word16) }

-- Have to redefine these so they're not exported as record fields.

getJobs :: EcloguesClient -> Result [JobStatus]
getJobs = _getJobs
getJobStatus :: EcloguesClient -> Name -> Result JobStatus
getJobStatus = _getJobStatus
getJobState :: EcloguesClient -> Name -> Result JobState
getJobState = _getJobState
setJobState :: EcloguesClient -> Name -> JobState -> Result ()
setJobState = _setJobState
deleteJob :: EcloguesClient -> Name -> Result ()
deleteJob = _deleteJob
createJob :: EcloguesClient -> JobSpec -> Result ()
createJob = _createJob
getHealth :: EcloguesClient -> Result Health
getHealth = _getHealth
masterHost :: EcloguesClient -> (String, Word16)
masterHost = _masterHost

-- | Lookup the Eclogues master and return a set of functions for interacting
-- with it.
ecloguesClient :: ManagedZK -> ExceptT (ZookeeperError String) IO (Maybe EcloguesClient)
ecloguesClient = mkClient <=< getEcloguesLeader where
    mkClient :: Maybe (String, Word16) -> ExceptT (ZookeeperError String) IO (Maybe EcloguesClient)
    mkClient hostM = pure . flip fmap hostM $ \(host, port) ->
        let (     getJobs'
             :<|> jobStatus'
             :<|> jobState'
             :<|> sJobState'
             :<|> deleteJob'
             :<|> _  -- scheduler redirect
             :<|> _  -- output redirect
             :<|> createJob'
             :<|> getHealth') = client (Proxy :: Proxy VAPI) (BaseUrl Http host $ fromIntegral port)
        in EcloguesClient
            (err getJobs')
            (err . jobStatus')
            (err . jobState')
            (fmap err . sJobState')
            (err . deleteJob')
            (err . createJob')
            (err getHealth')
            (host, port)
    err = withExceptT tryParseErr . ExceptT . runEitherT
    tryParseErr :: ServantError -> Either ServantError JobError
    tryParseErr serr@(FailureResponse _ _ bs) = mapLeft (const serr) $ eitherDecode bs
    tryParseErr other                         = Left other

-- | Query Zookeeper for the Eclogues master host details, if any.
getEcloguesLeader :: ManagedZK -> ExceptT (ZookeeperError String) IO (Maybe (String, Word16))
getEcloguesLeader mzk = getLeaderInfo parse mzk "/eclogues" where
    parse Nothing   = Left "missing node content"
    parse (Just bs) = eitherDecodeStrict bs
