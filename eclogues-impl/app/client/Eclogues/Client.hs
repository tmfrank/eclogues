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
    , getJobs, getJobStatus, getJobStage, getHealth, masterHost
    -- ** Mutate
    , createJob, deleteJob, setJobStage
    -- * Zookeeper
    , getEcloguesLeader
    ) where

import Database.Zookeeper.Election (ZKError, getLeaderInfo)
import Database.Zookeeper.ManagedEvents (ManagedZK)
import Eclogues.API (API, JobError, Health)
import Eclogues.ServantInstances ()
import qualified Eclogues.Job as Job

import Control.Monad ((<=<))
import Control.Monad.Trans.Either (runEitherT)
import Control.Monad.Trans.Except (ExceptT (..), throwE, withExceptT)
import Data.Aeson (eitherDecode, eitherDecodeStrict)
import Data.Either.Combinators (mapLeft)
import Data.Proxy (Proxy (..))
import Data.Word (Word16)
import Servant.API ((:<|>) (..))
import Servant.Client (BaseUrl (..), Scheme (Http), ServantError (..), client)

type Result a = ExceptT (Either ServantError JobError) IO a

-- | Functions for interacting with an Eclogues master. See 'ecloguesClient'.
data EcloguesClient = EcloguesClient { _getJobs      ::             Result [Job.Status]
                                     , _getJobStatus :: Job.Name -> Result Job.Status
                                     , _getJobStage  :: Job.Name -> Result Job.Stage
                                     , _setJobStage  :: Job.Name -> Job.Stage -> Result ()
                                     , _deleteJob    :: Job.Name -> Result ()
                                     , _createJob    :: Job.Spec -> Result ()
                                     , _getHealth    ::             Result Health
                                     , _masterHost   :: (String, Word16) }

-- Have to redefine these so they're not exported as record fields.

getJobs      :: EcloguesClient -> Result [Job.Status]
getJobs       = _getJobs
getJobStatus :: EcloguesClient -> Job.Name -> Result Job.Status
getJobStatus  = _getJobStatus
getJobStage  :: EcloguesClient -> Job.Name -> Result Job.Stage
getJobStage   = _getJobStage
setJobStage  :: EcloguesClient -> Job.Name -> Job.Stage -> Result ()
setJobStage   = _setJobStage
deleteJob    :: EcloguesClient -> Job.Name -> Result ()
deleteJob     = _deleteJob
createJob    :: EcloguesClient -> Job.Spec -> Result ()
createJob     = _createJob
getHealth    :: EcloguesClient -> Result Health
getHealth     = _getHealth
masterHost   :: EcloguesClient -> (String, Word16)
masterHost    = _masterHost

-- | Lookup the Eclogues master and return a set of functions for interacting
-- with it.
ecloguesClient :: ManagedZK -> ExceptT (Either ZKError String) IO (Maybe EcloguesClient)
ecloguesClient = mkClient <=< getEcloguesLeader where
    mkClient :: Maybe (String, Word16) -> ExceptT (Either ZKError String) IO (Maybe EcloguesClient)
    mkClient hostM = pure . flip fmap hostM $ \(host, port) ->
        let (     getJobs'
             :<|> jobStatus'
             :<|> jobStage'
             :<|> sJobStage'
             :<|> deleteJob'
             :<|> _  -- scheduler redirect
             :<|> _  -- output redirect
             :<|> createJob'
             :<|> getHealth') = client (Proxy :: Proxy API) (BaseUrl Http host $ fromIntegral port)
        in EcloguesClient
            (err getJobs')
            (err . jobStatus')
            (err . jobStage')
            (fmap err . sJobStage')
            (err . deleteJob')
            (err . createJob')
            (err getHealth')
            (host, port)
    err = withExceptT tryParseErr . ExceptT . runEitherT
    tryParseErr :: ServantError -> Either ServantError JobError
    tryParseErr serr@(FailureResponse _ _ bs) = mapLeft (const serr) $ eitherDecode bs
    tryParseErr other                         = Left other

-- | Query Zookeeper for the Eclogues master host details, if any.
getEcloguesLeader :: ManagedZK -> ExceptT (Either ZKError String) IO (Maybe (String, Word16))
getEcloguesLeader mzk = parse =<< withExceptT Left (getLeaderInfo mzk "/eclogues") where
    parse Nothing          = pure Nothing
    parse (Just Nothing)   = throwE $ Right "missing node content"
    parse (Just (Just bs)) = either (throwE . Right) pure $ eitherDecodeStrict bs
