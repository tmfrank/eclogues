{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Eclogues.Scheduling.AuroraZookeeper (getAuroraMaster, followAuroraMaster) where

import Database.Zookeeper.Election (ZookeeperError, followLeaderInfo, getLeaderInfo)
import Database.Zookeeper.ManagedEvents (ZNode, ManagedZK)

import Control.Arrow ((&&&))
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.AdvSTM (AdvSTM, atomically, newTVar, readTVar)
import Control.Monad (join)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (eitherDecodeStrict)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString (ByteString)
import Data.Word (Word16)
import Database.Zookeeper (ZKError)
import Network.URI (URI, parseURI)

type AuroraHost = (String, Word16)

data AuroraMember = AuroraMember { serviceEndpoint :: AuroraEndpoint } deriving (Show)
data AuroraEndpoint = AuroraEndpoint { host :: String, port :: Word16 } deriving (Show)

$(deriveJSON defaultOptions ''AuroraMember)
$(deriveJSON defaultOptions ''AuroraEndpoint)

conv :: Maybe ByteString -> Either String AuroraHost
conv Nothing   = Left "missing node content"
conv (Just bs) = (host &&& port) . serviceEndpoint <$> eitherDecodeStrict bs

rightMay :: Either e a -> Maybe a
rightMay (Left  _) = Nothing
rightMay (Right a) = Just a

getAuroraMaster :: ManagedZK -> ZNode -> ExceptT (ZookeeperError String) IO (Maybe AuroraHost)
getAuroraMaster = getLeaderInfo conv

followAuroraMaster :: ManagedZK -> ZNode -> IO (Async ZKError, AdvSTM (Maybe URI))
followAuroraMaster zk node = go where
    go = do
        var <- atomically $ newTVar Nothing
        followThread <- async $ followLeaderInfo zk node var
        return (followThread, getURI var)
    getURI var = readTVar var >>= \case
        Nothing  -> return Nothing
        Just bsM -> return . join . rightMay $ toURI <$> conv bsM
    toURI (host', port') = parseURI $ "http://" ++ host' ++ ':':(show port') ++ "/api"
