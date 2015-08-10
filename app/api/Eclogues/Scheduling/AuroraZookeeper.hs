{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Retrieving Aurora master details from Zookeeper.
-}

module Eclogues.Scheduling.AuroraZookeeper (
    AuroraHost, getAuroraMaster, followAuroraMaster) where

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

-- | Hostname and port of an Aurora master.
type AuroraHost = (String, Word16)

-- | Aurora ZK election nodes contain this JSON.
data AuroraMember = AuroraMember { serviceEndpoint :: AuroraEndpoint } deriving (Show)
data AuroraEndpoint = AuroraEndpoint { host :: String, port :: Word16 } deriving (Show)

$(deriveJSON defaultOptions ''AuroraMember)
$(deriveJSON defaultOptions ''AuroraEndpoint)

-- | Try to extract host details from Aurora ZK election node data.
conv :: Maybe ByteString -> Either String AuroraHost
conv Nothing   = Left "missing node content"
conv (Just bs) = (host &&& port) . serviceEndpoint <$> eitherDecodeStrict bs

rightMay :: Either e a -> Maybe a
rightMay (Left  _) = Nothing
rightMay (Right a) = Just a

-- | Retrieve host details of the elected Aurora master, if any.
getAuroraMaster :: ManagedZK -> ZNode -> ExceptT (ZookeeperError String) IO (Maybe AuroraHost)
getAuroraMaster = getLeaderInfo conv

-- | Follow the elected Aurora master asynchronously. Spawns a thread that will
-- run until a Zookeeper error occurs. Master details available via an 'AdvSTM'
-- action.
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
