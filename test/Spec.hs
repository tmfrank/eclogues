module Main where

import Units

import Test.Hspec

spec :: Spec
spec =
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

main :: IO ()
main = hspec spec
