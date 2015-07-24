module Eclogues.Client ( EcloguesClient ( getJobs
                                        , getJobStatus
                                        , getJobState
                                        , setJobState
                                        , deleteJob
                                        , createJob
                                        , getHealth
                                        , masterHost )
                       , Result, getEcloguesLeader, ecloguesClient ) where

import Database.Zookeeper.Election (ZookeeperError, getLeaderInfo)
import Database.Zookeeper.ManagedEvents (ManagedZK)
import Eclogues.API (VAPI, JobError, Health)
import Eclogues.Client.Instances ()
import Eclogues.Instances ()
import Eclogues.TaskSpec (JobStatus, JobState, Name, TaskSpec)

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

data EcloguesClient = EcloguesClient { getJobs      ::             Result [JobStatus]
                                     , getJobStatus :: Name     -> Result JobStatus
                                     , getJobState  :: Name     -> Result JobState
                                     , setJobState  :: Name     -> JobState -> Result ()
                                     , deleteJob    :: Name     -> Result ()
                                     , createJob    :: TaskSpec -> Result ()
                                     , getHealth    ::             Result Health
                                     , masterHost   :: (String, Word16) }

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

getEcloguesLeader :: ManagedZK -> ExceptT (ZookeeperError String) IO (Maybe (String, Word16))
getEcloguesLeader mzk = getLeaderInfo parse mzk "/eclogues" where
    parse Nothing   = Left "missing node content"
    parse (Just bs) = eitherDecodeStrict bs
