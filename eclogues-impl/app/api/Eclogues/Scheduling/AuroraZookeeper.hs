{-# LANGUAGE FlexibleContexts #-}
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
    AuroraHost, followAuroraMaster) where

import Database.Zookeeper.Election (followLeaderInfo)
import Database.Zookeeper.ManagedEvents (ManagedZK)

import Control.Concurrent.AdvSTM (AdvSTM, atomically, newTVar, readTVar)
import Control.Monad (join)
import Data.Aeson (decodeStrict')
import Data.Aeson.TH (deriveJSON, defaultOptions)
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

-- | Follow the elected Aurora master asynchronously. Returns an IO action to
-- run until a Zookeeper error occurs. Master details available via an 'AdvSTM'
-- action.
followAuroraMaster :: ManagedZK -> IO (IO ZKError, AdvSTM (Maybe URI))
followAuroraMaster zk = go where
    go = do
        var <- atomically $ newTVar Nothing
        let followAction = followLeaderInfo zk "/aurora/scheduler" var
        pure (followAction, getURI var)
    getURI var = (parse =<<) . join <$> readTVar var
    parse = (toURI =<<) . fmap serviceEndpoint . decodeStrict'
    toURI ep = parseURI $ "http://" ++ host ep ++ ':' : show (port ep) ++ "/api"
