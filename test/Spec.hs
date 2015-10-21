{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Shannon Pace <space@swin.edu.au>
Stability   : unstable
Portability : portable

Test entry point.
-}


module Main where

import Api_Types

import qualified Eclogues.Job as Job
import Eclogues.Scheduling.AuroraConfig
import MockSpec (testMock)
import StateSpec
import MonitorSpec

import Test.Hspec
import Test.QuickCheck (property, counterexample)

import Data.Aeson (decode, eitherDecode, encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Maybe (isJust, fromMaybe)
import Data.Metrology.Computing (Byte (Byte), Core (Core), (%>))
import Data.Metrology.SI (Second (Second), mega, centi)
import Data.Text.Lazy.Encoding (encodeUtf8)

testThrift :: Spec
testThrift = do
    let task = Job.Spec tn "/bin/echo" res [] False []
        tn   = fromMaybe (error "hello is not a valid job name") $ Job.mkName "hello"
        res  = fromMaybe (error "hardcoded resources are invalid") $
                   Job.mkResources (10 %> mega Byte) (10 %> mega Byte) (10 %> centi Core) (5 %> Second)

    describe "ATaskExecConf" $
        it "is embedded in a JobConfiguration" $ do
            let Just ec = taskConfig_executorConfig . jobConfiguration_taskConfig $ auroraJobConfig "default" task
            let text = executorConfig_data ec
            let encoded = encodeUtf8 text
            (decode encoded :: Maybe ATaskExecConf) `shouldSatisfy` isJust

testJson :: Spec
testJson = describe "Resources" $
    it "round trips through JSON" $ property $ \(x :: Job.Resources) ->
        let encoded = encode x
            decoded = eitherDecode encoded
        in counterexample (" => " ++ unpack encoded ++ "\n => " ++ show decoded)
         $ either (const False) (== x) decoded

main :: IO ()
main = hspec $ do
    testThrift
    testJson
    testState
    testMock
    testMonitor
