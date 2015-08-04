module Eclogues.JobSpec.Aeson where

import Data.Char (toLower)

specJName :: String -> String
specJName "_job_resources" = "resources"
specJName x                = fmap toLower $ drop 1 x

statusJName :: String -> String
statusJName "_job_spec" = "spec"
statusJName "_jobState" = "state"
statusJName x           = fmap toLower $ drop 1 x
