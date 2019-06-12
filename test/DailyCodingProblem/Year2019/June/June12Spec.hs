module DailyCodingProblem.Year2019.June.June12Spec (spec) where

import Test.Hspec
import DailyCodingProblem.Year2019.June.June12.Solution

spec :: Spec
spec =
    describe "firstMissingPosInt" $ do
        it "handles an empty list" $
            firstMissingPosInt [] `shouldBe` 1

        it "handles only negative numbers" $
            firstMissingPosInt [-5, -10, -1] `shouldBe` 1

        it "handles a small list with numbers" $
            firstMissingPosInt [12, 4, 1, 2, 9] `shouldBe` 3
