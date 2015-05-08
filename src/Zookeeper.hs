{-# LANGUAGE TemplateHaskell #-}

module Zookeeper where

import Control.Applicative ((<$>), pure)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), withExceptT, throwE, runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Aeson (eitherDecodeStrict)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.List (isPrefixOf, sort)
import Data.Maybe (listToMaybe)
import Data.Word (Word16)
import Database.Zookeeper (Zookeeper, ZKError (..), withZookeeper, getChildren, get)

type ZKURI = String
type ZNode = String
type AuroraHost = (String, Word16)

data ZookeeperError = ContentError String
                    | ZookeeperError ZKError
                    deriving (Show)

data AuroraMember = AuroraMember { serviceEndpoint :: AuroraEndpoint } deriving (Show)
data AuroraEndpoint = AuroraEndpoint { host :: String, port :: Word16 } deriving (Show)

$(deriveJSON defaultOptions ''AuroraMember)
$(deriveJSON defaultOptions ''AuroraEndpoint)

getAuroraMaster :: ZKURI -> ZNode -> ExceptT ZookeeperError IO (Maybe AuroraHost)
getAuroraMaster zkUri node = ExceptT $ withZookeeper zkUri 1000 Nothing Nothing (runExceptT . runMaybeT . fetch) where
    fetch :: Zookeeper -> MaybeT (ExceptT ZookeeperError IO) AuroraHost
    fetch zk = do
        children <- MaybeT . ExceptT $ maybeExists <$> getChildren zk node Nothing
        first <- MaybeT . pure . listToMaybe . sort $ filter (isPrefixOf "member_") children
        nodeM <- lift . ExceptT $ maybeExists <$> get zk (node ++ "/" ++ first) Nothing
        case nodeM of
            Nothing           -> fetch zk
            Just (Nothing, _) -> lift . throwE $ ContentError "missing node content"
            Just (Just bs, _) -> lift $ do
                member <- withExceptT ContentError . ExceptT . pure $ eitherDecodeStrict bs
                let endpoint = serviceEndpoint member
                pure $ (host endpoint, port endpoint)
    maybeExists :: Either ZKError a -> Either ZookeeperError (Maybe a)
    maybeExists (Right a)          = Right (Just a)
    maybeExists (Left NoNodeError) = Right Nothing
    maybeExists (Left e)           = Left (ZookeeperError e)
