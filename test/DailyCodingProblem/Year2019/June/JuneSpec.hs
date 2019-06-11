module DailyCodingProblem.Year2019.June.JuneSpec (spec) where

import Test.Hspec
import qualified DailyCodingProblem.Year2019.June.June9 as J9
import qualified DailyCodingProblem.Year2019.June.June10 as J10

spec :: Spec
spec = do
    describe "June 9th" $
        describe "twoAddsUpToK" $ do
            it "returns true when two values add up to k" $
                J9.twoAddsUpToK 5 [5, 0] `shouldBe` True
            it "returns true when two identical values add up to k" $
                J9.twoAddsUpToK 4 [2, 2] `shouldBe` True
            it "can't use same element twice" $
                J9.twoAddsUpToK 2 [1, 3] `shouldBe` False
    
    describe "June 10th" $ do
        describe "productArray" $
            it "returns an array according to spec" $
                J10.productArray [1, 2, 3, 4] `shouldBe` [24, 12, 8, 6]
        describe "sumArray" $
            it "returns an array according to spec" $
                J10.sumArray [1, 2, 3, 4] `shouldBe` [9, 8, 7, 6]
        describe "concatArray" $
            it "returns an array according to spec" $
                J10.concatArray [[1], [2], [3], [4]]
                    `shouldBe` [ [2, 3, 4]
                               , [1, 3, 4]
                               , [1, 2, 4]
                               , [1, 2, 3] ]