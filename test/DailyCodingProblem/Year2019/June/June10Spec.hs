module DailyCodingProblem.Year2019.June.June10Spec (spec) where

import Test.Hspec
import DailyCodingProblem.Year2019.June.June10.Solution

spec :: Spec
spec = do
    describe "productArray" $
        it "returns an array according to spec" $
            productArray [1, 2, 3, 4] `shouldBe` [24, 12, 8, 6]

    describe "sumArray" $
        it "returns an array according to spec" $
            sumArray [1, 2, 3, 4] `shouldBe` [9, 8, 7, 6]

    describe "concatArray" $
        it "returns an array according to spec" $
            concatArray [[1], [2], [3], [4]]
                `shouldBe` [ [2, 3, 4]
                           , [1, 3, 4]
                           , [1, 2, 4]
                           , [1, 2, 3] ]

