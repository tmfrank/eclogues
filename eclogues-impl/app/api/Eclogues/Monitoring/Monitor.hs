{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Shannon Pace <space@swin.edu.au>
Stability   : unstable
Portability : portable

Client for Aegle API consuming machine resource data.
-}

module Eclogues.Monitoring.Monitor (slaveResources, followAegleMaster) where

import Eclogues.Monitoring.Cluster (Cluster, NodeResources (..))

import qualified Aegle.API as A
import Control.Concurrent.AdvSTM (AdvSTM, atomically, newTVar, readTVar)
import Control.Lens ((^.), view)
import Control.Monad (join)
import Control.Monad.Trans.Either (EitherT)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Database.Zookeeper (ZKError)
import Database.Zookeeper.Election (followLeaderInfo)
import Database.Zookeeper.ManagedEvents (ManagedZK)
import Servant.API ((:<|>) ((:<|>)))
import Servant.Client (client)
import Servant.Common.BaseUrl (BaseUrl (..), Scheme (Http))
import Servant.Common.Req (ServantError)

toNodeRes :: A.Resources -> Maybe NodeResources
toNodeRes res = NodeResources <$> res ^. A.disk
                              <*> res ^. A.ram
                              <*> res ^. A.cpu

slaveResources :: BaseUrl -> EitherT ServantError IO Cluster
slaveResources host = toCluster <$> getRes [] ["mesos-slave"]
    where
        (_ :<|> getRes :<|> _) = client (Proxy :: Proxy A.API) host
        toCluster = catMaybes . fmap (toNodeRes . view A.total) . HM.elems


-- | Follow the elected Aegle master asynchronously. Returns an IO action to
-- run until a Zookeeper error occurs. Master details available via an 'AdvSTM'
-- action.
followAegleMaster :: ManagedZK -> IO (IO ZKError, AdvSTM (Maybe BaseUrl))
followAegleMaster zk = do
    var <- atomically $ newTVar Nothing
    let followThread = followLeaderInfo zk A.zkNode var
    pure (followThread, getUrl var)
  where
    getUrl var = fmap toURI . (A.parseZKData =<<) . join <$> readTVar var
    toURI (host, port) = BaseUrl Http host (fromIntegral port)
