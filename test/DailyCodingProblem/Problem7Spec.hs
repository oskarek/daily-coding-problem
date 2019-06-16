{-# LANGUAGE OverloadedLists #-}
module DailyCodingProblem.Problem7Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import DailyCodingProblem.Problem7.Solution

spec :: Spec
spec = do
    describe "possibleDecodings" $ do
        it "handles empty messages" $
            possibleDecodings [] `shouldBe` [""]

        it "gives {'ka', 'ak', 'aaa'} as result for message '111'" $
            possibleDecodings "111" `shouldBe` ["ka", "ak", "aaa"]

    describe "numberOfDecodings" $ do
        it "handles empty messages" $
            numberOfDecodings [] `shouldBe` 1

        it "gives 3 as result for message '111'" $
            numberOfDecodings "111" `shouldBe` 3

    describe "numberOfDecodings and numberOfDecodings'" $
        prop "they are equal" $
            \message -> numberOfDecodings message == numberOfDecodings' message