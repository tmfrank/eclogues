{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Eclogues.Monitoring.Cluster (Cluster, NodeResources(..))
import qualified Units as U

import Control.Monad.Trans.Either (EitherT)
import Data.Aeson.TH (Options(fieldLabelModifier), defaultOptions, deriveFromJSON)
import Data.Char (toLower)
import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Lazy (HashMap)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack)
import Servant.API ((:>), Get, JSON, QueryParams)
import Servant.Client (client)
import Servant.Common.BaseUrl (BaseUrl(..))
import Servant.Common.Req (ServantError)

type Host = Text
type Group = Text

data MonitorRes = MonitorRes { mDisk :: Maybe Double
                             , mRam  :: Maybe Double
                             , mCpu  :: Maybe Double }
                             deriving (Show, Eq)

$(deriveFromJSON defaultOptions { fieldLabelModifier = fmap toLower . drop 1 } ''MonitorRes)

type ResMap = HashMap Host MonitorRes

type HealthAPI = "resources" :> QueryParams "hosts" Host :> QueryParams "groups" Group :> Get '[JSON] ResMap

toNodeRes :: MonitorRes -> Maybe NodeResources
toNodeRes res = NodeResources <$> d <*> r <*> c
    where
        d = U.mega U.byte <$> mDisk res
        r = U.mebi U.byte <$> mRam res
        c = U.core <$> mCpu res

slaveResources :: BaseUrl -> EitherT ServantError IO Cluster
slaveResources host = toCluster <$> getRes [] [pack "mesos-slave"]
    where
        getRes = client (Proxy::Proxy HealthAPI) host
        toCluster = catMaybes . fmap toNodeRes . HM.elems
