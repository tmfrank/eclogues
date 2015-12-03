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
      EcloguesClient, Result, mkClient, ecloguesClient
    -- ** View
    , getJobs, getJobStatus, getJobStage, getHealth, baseUrl
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
                                     , _baseUrl      :: BaseUrl }

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
baseUrl      :: EcloguesClient -> BaseUrl
baseUrl       = _baseUrl

mkClient :: BaseUrl -> EcloguesClient
mkClient url =
    EcloguesClient
        (err getJobs')
        (err . jobStatus')
        (err . jobStage')
        (fmap err . sJobStage')
        (err . deleteJob')
        (err . createJob')
        (err getHealth')
        url
  where
    (     getJobs'
     :<|> jobStatus'
     :<|> jobStage'
     :<|> sJobStage'
     :<|> deleteJob'
     :<|> _  -- scheduler redirect
     :<|> _  -- output redirect
     :<|> createJob'
     :<|> getHealth') = client (Proxy :: Proxy API) url
    err = withExceptT tryParseErr . ExceptT . runEitherT
    tryParseErr :: ServantError -> Either ServantError JobError
    tryParseErr serr@(FailureResponse _ _ bs) = mapLeft (const serr) $ eitherDecode bs
    tryParseErr other                         = Left other

-- | Lookup the Eclogues master and return a set of functions for interacting
-- with it.
ecloguesClient :: ManagedZK -> ExceptT (Either ZKError String) IO (Maybe EcloguesClient)
ecloguesClient = go <=< getEcloguesLeader where
    go :: Maybe (String, Word16) -> ExceptT (Either ZKError String) IO (Maybe EcloguesClient)
    go hostM = pure . flip fmap hostM $
        \(host, port) -> mkClient $ BaseUrl Http host $ fromIntegral port

-- | Query Zookeeper for the Eclogues master host details, if any.
getEcloguesLeader :: ManagedZK -> ExceptT (Either ZKError String) IO (Maybe (String, Word16))
getEcloguesLeader mzk = parse =<< withExceptT Left (getLeaderInfo mzk "/eclogues") where
    parse Nothing          = pure Nothing
    parse (Just Nothing)   = throwE $ Right "missing node content"
    parse (Just (Just bs)) = either (throwE . Right) pure $ eitherDecodeStrict bs
