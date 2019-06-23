module DailyCodingProblem.Problem13Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import DailyCodingProblem.Problem13.Solution
import qualified Data.Vector as V

spec :: Spec
spec = do
    describe "longestKSLength" $ do
        it "returns 3 for 'abcba' when k = 2" $
            longestKSLength 2 "abcba" `shouldBe` 3

        prop "always returns 0 for empty strings" $
            \k -> longestKSLength k "" `shouldBe` 0

    describe "longestKSLength'" $
        prop "longestKSLength' == longestKSLength" $ forAll (choose (0,500))
            $ \n -> forAll (vectorOf n arbitrary) $
                \cs -> longestKSLength' n (V.fromList cs) == longestKSLength n cs

    
    describe "suffixes" $ do
        it "handles an empty list" $
            suffixes ([] :: [Int]) `shouldBe` [[]]
        
        it "gives all possible suffixes for list [1,2,3]" $
            suffixes ([1,2,3] :: [Int]) `shouldBe` [[1,2,3],[2,3],[3],[]]
