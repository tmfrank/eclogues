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

module Eclogues.AppConfig (AppConfig (..), requireSchedConf) where

import Eclogues.API (AbsFile)
import qualified Eclogues.Persist as Persist
import Eclogues.Scheduling.Command (ScheduleCommand, ScheduleConf (ScheduleConf), AuroraURI)
import qualified Eclogues.Job as Job

import Control.Concurrent.AdvSTM (AdvSTM, retry)
import Control.Concurrent.AdvSTM.TChan (TChan)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Data.UUID (UUID)
import Network.URI (URI)
import Path (Path, Abs, Dir)

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
                           -- | URL for Graphite server monitoring cluster.
                           , monitorUrl      :: String }

-- | Wait until the scheduler is available.
requireSchedConf :: AppConfig -> AdvSTM ScheduleConf
requireSchedConf conf = ScheduleConf (jobsDir conf) (fromStrict $ subexecutorUser conf) <$> requireAurora conf

requireAurora :: AppConfig -> AdvSTM AuroraURI
requireAurora conf = do
    uriM <- auroraURI conf
    case uriM of
        Just uri -> return uri
        Nothing  -> retry
