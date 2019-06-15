module DailyCodingProblem.Problem1Spec (spec) where

import Test.Hspec
import DailyCodingProblem.Problem1.Solution

spec :: Spec
spec =
    describe "twoAddsUpToK" $ do
        it "returns true when two values add up to k" $
            twoAddsUpToK 5 [5, 0] `shouldBe` True

        it "returns true when two identical values add up to k" $
            twoAddsUpToK 4 [2, 2] `shouldBe` True

        it "can't use same element twice" $
            twoAddsUpToK 2 [1, 3] `shouldBe` False
