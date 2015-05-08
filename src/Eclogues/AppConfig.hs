module Eclogues.AppConfig where

import Network.URI (URI)

data AppConfig = AppConfig { jobsDir   :: FilePath
                           , auroraURI :: URI }
