{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api_Types

import Eclogues.Scheduling.AuroraConfig
import Eclogues.JobSpec
import Units
import StateSpec

import Test.Hspec

import Data.Aeson (decode)
import Data.Maybe (isJust)
import Data.Text.Lazy.Encoding (encodeUtf8)

testThrift :: Spec
testThrift = do
    let task = JobSpec "hello" "/bin/echo" (Resources (mega byte 10) (mebi byte 10) (core 0.1) (second 5)) [] False []

    describe "ATaskExecConf" $
        it "is embedded in a JobConfiguration" $ do
            let Just ec = taskConfig_executorConfig . jobConfiguration_taskConfig $ auroraJobConfig "default" $ task
            let text = executorConfig_data ec
            let encoded = encodeUtf8 text
            (decode encoded :: Maybe ATaskExecConf) `shouldSatisfy` isJust

testUnits :: Spec
testUnits = do
    describe "Units" $ do
        it "keeps val" $
            val (mega byte 10) `shouldBe` (10 :: Double)

        it "converts MB to MB" $ do
            mega byte 1 `as` mega byte `shouldBe` mega byte (1 :: Double)
            mega byte 1 `asVal` mega byte `shouldBe` (1 :: Double)

        it "converts MB to bytes" $
            mega byte 1 `asVal` byte `shouldBe` (1e6 :: Double)

        it "converts bytes to MB" $
            byte 1e6 `asVal` mega byte `shouldBe` (1 :: Double)

        it "converts seconds to microseconds" $
            second 15 `asVal` micro second `shouldBe` (15e6 :: Double)

main :: IO ()
main = hspec $ do
    testThrift
    testUnits
    testState
