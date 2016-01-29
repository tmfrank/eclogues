{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

API implementation global config, static from initial generation.
-}

module Eclogues.AppConfig (AppConfig (..)) where

import Eclogues.API (AbsFile)
import qualified Eclogues.Persist as Persist
import Eclogues.Scheduling.Command (ScheduleCommand)
import qualified Eclogues.Job as Job

import Control.Concurrent.AdvSTM (AdvSTM)
import Control.Concurrent.AdvSTM.TChan (TChan)
import Data.Text (Text)
import Data.UUID (UUID)
import Network.URI (URI)
import Path (Path, Abs, Dir)
import Servant.Common.BaseUrl (BaseUrl)

data AppConfig = AppConfig {
                           -- | The mount point of the shared jobs directory.
                             jobsDir         :: Path Abs Dir
                           -- | Get the URI to the scheduler, if it's available.
                           , auroraURI       :: AdvSTM (Maybe URI)
                           , schedChan       :: TChan ScheduleCommand
                           , pctx            :: Persist.Context
                           -- | Scheduler web UI for the provided job UUID.
                           , schedJobURI     :: URI -> UUID -> URI
                           -- | Job file output URI.
                           , outputURI       :: Job.Name -> AbsFile -> URI
                           -- | User the subexecutor is run as.
                           , subexecutorUser :: Text
                           -- | URL for Aegle server monitoring cluster.
                           , monitorUrl      :: AdvSTM (Maybe BaseUrl) }
