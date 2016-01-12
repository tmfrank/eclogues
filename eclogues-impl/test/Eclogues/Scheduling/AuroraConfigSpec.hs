{-# LANGUAGE OverloadedStrings #-}

module Eclogues.Scheduling.AuroraConfigSpec (spec) where

import Api_Types (
    taskConfig_executorConfig, jobConfiguration_taskConfig, executorConfig_data)
import qualified Eclogues.Job as Job
import Eclogues.Scheduling.AuroraConfig (ATaskExecConf, auroraJobConfig)

import Data.Aeson (decode)
import Data.Maybe (isJust, fromMaybe)
import Data.Metrology.Computing (Byte (Byte), Core (Core), (%>))
import Data.Metrology.SI (Second (Second), mega, centi)
import Data.Text.Lazy.Encoding (encodeUtf8)

import Test.Hspec

{-# ANN module ("HLint: ignore Use ." :: String) #-}

spec :: Spec
spec = do
    let task = Job.mkSpec tn "/bin/echo" res [] False []
        tn   = fromMaybe (error "hello is not a valid job name") $ Job.mkName "hello"
        res  = fromMaybe (error "hardcoded resources are invalid") $
                   Job.mkResources (10 %> mega Byte) (10 %> mega Byte) (10 %> centi Core) (5 %> Second)

    describe "ATaskExecConf" $
        it "is embedded in a JobConfiguration" $ do
            let Just ec = taskConfig_executorConfig . jobConfiguration_taskConfig $ auroraJobConfig "default" task
            let text = executorConfig_data ec
            let encoded = encodeUtf8 text
            (decode encoded :: Maybe ATaskExecConf) `shouldSatisfy` isJust
