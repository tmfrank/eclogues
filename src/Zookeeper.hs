{-# LANGUAGE TemplateHaskell #-}

module Zookeeper where

import Control.Applicative ((<$>), pure)
import Control.Arrow ((&&&))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), withExceptT, runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Aeson (eitherDecodeStrict)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString (ByteString)
import Data.List (isPrefixOf, sort)
import Data.Maybe (listToMaybe)
import Data.Word (Word16)
import Database.Zookeeper (Zookeeper, ZKError (..), withZookeeper, getChildren, get)

type ZKURI = String
type ZNode = String
type AuroraHost = (String, Word16)

data ZookeeperError e = ContentError e
                      | ZookeeperError ZKError
                      deriving (Show)

data AuroraMember = AuroraMember { serviceEndpoint :: AuroraEndpoint } deriving (Show)
data AuroraEndpoint = AuroraEndpoint { host :: String, port :: Word16 } deriving (Show)

$(deriveJSON defaultOptions ''AuroraMember)
$(deriveJSON defaultOptions ''AuroraEndpoint)

getLeaderInfo :: forall e a. (Maybe ByteString -> Either e a) -> ZKURI -> ZNode -> ExceptT (ZookeeperError e) IO (Maybe a)
getLeaderInfo conv zkUri node = ExceptT $ withZookeeper zkUri 1000 Nothing Nothing (runExceptT . runMaybeT . fetch) where
    fetch :: Zookeeper -> MaybeT (ExceptT (ZookeeperError e) IO) a
    fetch zk = do
        children <- MaybeT . ExceptT $ maybeExists <$> getChildren zk node Nothing
        first <- MaybeT . pure . listToMaybe . sort $ filter (isPrefixOf "member_") children
        nodeM <- lift . ExceptT $ maybeExists <$> get zk (node ++ "/" ++ first) Nothing
        case nodeM of
            Nothing       -> fetch zk
            Just (bsM, _) -> lift . withExceptT ContentError . ExceptT . pure $ conv bsM
    maybeExists :: Either ZKError b -> Either (ZookeeperError e) (Maybe b)
    maybeExists (Right a)          = Right (Just a)
    maybeExists (Left NoNodeError) = Right Nothing
    maybeExists (Left e)           = Left (ZookeeperError e)

getAuroraMaster :: ZKURI -> ZNode -> ExceptT (ZookeeperError String) IO (Maybe AuroraHost)
getAuroraMaster = getLeaderInfo conv where
    conv :: Maybe ByteString -> Either String AuroraHost
    conv Nothing   = Left "missing node content"
    conv (Just bs) = (host &&& port) . serviceEndpoint <$> eitherDecodeStrict bs
