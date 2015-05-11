module Eclogues.Client ( EcloguesClient (getJobs), Result
                       , getEcloguesLeader, ecloguesClient ) where

import Eclogues.API (VAPI, JobError)
import Eclogues.Client.Instances ()
import Eclogues.Instances ()
import Eclogues.TaskSpec (JobStatus)
import Eclogues.Zookeeper (ZKURI, ZookeeperError, getLeaderInfo)

import Control.Applicative (pure)
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

data EcloguesClient = EcloguesClient { getJobs :: Result [JobStatus] }

ecloguesClient :: ZKURI -> ExceptT (ZookeeperError String) IO (Maybe EcloguesClient)
ecloguesClient = mkClient <=< getEcloguesLeader where
    mkClient :: Maybe (String, Word16) -> ExceptT (ZookeeperError String) IO (Maybe EcloguesClient)
    mkClient hostM = pure . flip fmap hostM $ \(host, port) ->
        let getJobs' :<|> _ = client (Proxy :: Proxy VAPI) (BaseUrl Http host $ fromIntegral port)
        in EcloguesClient (err getJobs')
    err = withExceptT tryParseErr . ExceptT . runEitherT
    tryParseErr :: ServantError -> Either ServantError JobError
    tryParseErr serr@(FailureResponse _ _ bs) = mapLeft (const serr) $ eitherDecode bs
    tryParseErr other                         = Left other

getEcloguesLeader :: ZKURI -> ExceptT (ZookeeperError String) IO (Maybe (String, Word16))
getEcloguesLeader zkUri = getLeaderInfo parse zkUri "/eclogues" where
    parse Nothing   = Left "missing node content"
    parse (Just bs) = eitherDecodeStrict bs
