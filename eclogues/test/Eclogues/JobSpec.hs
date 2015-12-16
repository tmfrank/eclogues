{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eclogues.JobSpec (spec) where

import qualified Eclogues.Job as Job

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)
import Data.Maybe (fromMaybe)
import Data.Metrology.Computing (Byte (Byte), Core (Core), (%>))
import Data.Metrology.SI (Second (Second), mega, centi)
import Data.Scientific.Suspicious (Sustific, fromFloatDigits)
import NeatInterpolation
import Path (mkAbsFile)

import Test.Hspec
import Test.QuickCheck (
    Arbitrary (arbitrary), getNonZero, property, counterexample)

spec :: Spec
spec = do
    describe "Resources" $
        it "round trips through JSON" $ property $ \(x :: Job.Resources) ->
            let encoded = encode x
                decoded = eitherDecode encoded
            in counterexample (" => " ++ unpack encoded ++ "\n => " ++ show decoded)
             $ either (const False) (== x) decoded

    describe "Spec" $
        it "can be decoded from a particular string" $
            eitherDecode exampleEncoded `shouldBe` Right exampleSpec

exampleEncoded :: ByteString
exampleEncoded = pack $
    [string|
    {
        "name": "hello",
        "command": "echo hello world > hello.txt",
        "resources": {
            "disk": "10 MB",
            "ram": "10 MB",
            "cpu": "0.1 cores",
            "time": "5 s"
        },
        "outputfiles": ["/hello.txt"],
        "capturestdout": false,
        "dependson": []
    }
    |]

exampleSpec :: Job.Spec
exampleSpec = Job.mkSpec name "echo hello world > hello.txt" res ops False []
  where
    res  = fromMaybe (error "example resources failed") $
        Job.mkResources (10 %> mega Byte) (10 %> mega Byte) (10 %> centi Core) (5 %> Second)
    name = fromMaybe (error "example name failed") $ Job.mkName "hello"
    ops  = [Job.OutputPath $(mkAbsFile "/hello.txt")]

instance Arbitrary Job.Resources where
    arbitrary = mk <$> v (mega Byte) <*> v (mega Byte) <*> v Core <*> v Second
      where
        v t = (%> t) . dblToSus . pos . getNonZero <$> arbitrary
        pos :: Double -> Double
        pos = (+ 1) . abs
        dblToSus :: Double -> Sustific
        dblToSus = fromFloatDigits
        mk d r c t = fromMaybe (error "arb resources failed somehow") $ Job.mkResources d r c t

