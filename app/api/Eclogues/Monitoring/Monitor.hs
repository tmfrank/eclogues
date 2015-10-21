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

module Eclogues.Monitoring.Monitor (slaveResources) where

import Eclogues.Monitoring.Cluster (Cluster, NodeResources (..))

import qualified Aegle.API as A
import Control.Lens ((^.), view)
import Control.Monad.Trans.Either (EitherT)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Servant.API ((:<|>) ((:<|>)))
import Servant.Client (client)
import Servant.Common.BaseUrl (BaseUrl (..))
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
