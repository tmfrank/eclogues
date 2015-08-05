module Eclogues.AppConfig (AppConfig (..), requireSchedConf) where

import Eclogues.API (AbsFile)
import Eclogues.Persist (PersistContext)
import Eclogues.Scheduling.Command (ScheduleCommand, ScheduleConf (ScheduleConf), AuroraURI)
import Eclogues.JobSpec (Name)

import Control.Concurrent.AdvSTM (AdvSTM, retry)
import Control.Concurrent.AdvSTM.TChan (TChan)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Data.UUID (UUID)
import Network.URI (URI)

data AppConfig = AppConfig { jobsDir         :: FilePath
                           , auroraURI       :: AdvSTM (Maybe URI)
                           , schedChan       :: TChan ScheduleCommand
                           , pctx            :: PersistContext
                           , schedJobURI     :: URI -> UUID -> URI
                           , outputURI       :: Name -> AbsFile -> URI
                           , subexecutorUser :: Text }

requireSchedConf :: AppConfig -> AdvSTM ScheduleConf
requireSchedConf conf = ScheduleConf (jobsDir conf) (fromStrict $ subexecutorUser conf) <$> requireAurora conf

requireAurora :: AppConfig -> AdvSTM AuroraURI
requireAurora conf = do
    uriM <- auroraURI conf
    case uriM of
        Just uri -> return uri
        Nothing  -> retry
