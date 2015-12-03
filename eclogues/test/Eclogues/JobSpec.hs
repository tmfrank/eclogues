{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eclogues.JobSpec (spec) where

import qualified Eclogues.Job as Job

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Maybe (fromMaybe)
import Data.Metrology.Computing (Byte (Byte), Core (Core), (%>))
import Data.Metrology.SI (Second (Second), mega)
import Data.Scientific.Suspicious (Sustific, fromFloatDigits)

import Test.Hspec
import Test.QuickCheck (
    Arbitrary (arbitrary), getNonZero, property, counterexample)

spec :: Spec
spec = describe "Resources" $
    it "round trips through JSON" $ property $ \(x :: Job.Resources) ->
        let encoded = encode x
            decoded = eitherDecode encoded
        in counterexample (" => " ++ unpack encoded ++ "\n => " ++ show decoded)
         $ either (const False) (== x) decoded

instance Arbitrary Job.Resources where
    arbitrary = mk <$> v (mega Byte) <*> v (mega Byte) <*> v Core <*> v Second
      where
        v t = (%> t) . dblToSus . pos . getNonZero <$> arbitrary
        pos :: Double -> Double
        pos = (+ 1) . abs
        dblToSus :: Double -> Sustific
        dblToSus = fromFloatDigits
        mk d r c t = fromMaybe (error "arb resources failed somehow") $ Job.mkResources d r c t

