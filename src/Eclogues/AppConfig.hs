module Eclogues.AppConfig where

import Eclogues.Persist (PersistContext)
import Eclogues.Scheduling.Command (ScheduleCommand)
import Eclogues.TaskSpec (Name)

import Control.Concurrent.AdvSTM (AdvSTM, retry)
import Control.Concurrent.AdvSTM.TChan (TChan)
import Data.UUID (UUID)
import Network.URI (URI)

data AppConfig = AppConfig { jobsDir     :: FilePath
                           , auroraURI   :: AdvSTM (Maybe URI)
                           , schedChan   :: TChan ScheduleCommand
                           , pctx        :: PersistContext
                           , schedJobURI :: URI -> UUID -> String
                           , outputURI   :: Name -> FilePath -> String }

requireAurora :: AppConfig -> AdvSTM URI
requireAurora conf = do
    uriM <- auroraURI conf
    case uriM of
        Just uri -> return uri
        Nothing  -> retry
