module Eclogues.JobSpec (spec) where

import qualified Eclogues.Job as Job
import TestUtils ()

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy.Char8 (unpack)

import Test.Hspec
import Test.QuickCheck (property, counterexample)

spec :: Spec
spec = describe "Resources" $
    it "round trips through JSON" $ property $ \(x :: Job.Resources) ->
        let encoded = encode x
            decoded = eitherDecode encoded
        in counterexample (" => " ++ unpack encoded ++ "\n => " ++ show decoded)
         $ either (const False) (== x) decoded
