module DailyCodingProblem.Problem9Spec (spec) where

import Test.Hspec
import DailyCodingProblem.Problem9.Solution

spec :: Spec
spec =
    describe "largestNonAdjacentSum" $ do
        it "returns 0 for the empty list" $
            largestNonAdjacentSum [] `shouldBe` 0

        it "returns 13 for example list #1" $
            largestNonAdjacentSum [2, 4, 6, 2, 5] `shouldBe` 13

        it "returns 10 for example list #2" $
            largestNonAdjacentSum [5, 1, 1, 5] `shouldBe` 10

        it "should handle negative numbers" $
            largestNonAdjacentSum [-1, -4, -5, 8] `shouldBe` 8
            