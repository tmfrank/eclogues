module Eclogues.APIElectionSpec (spec) where

import Eclogues.API (parseZKData)
import Eclogues.APIElection (advertisedData)

import Test.Hspec
import Test.QuickCheck (property)

spec :: Spec
spec = describe "advertisedData" $
    it "is the inverse of parseZKData" $ property $ \(h, p) ->
        parseZKData (advertisedData h p) `shouldBe` Just (h, p)
