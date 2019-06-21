{-# LANGUAGE OverloadedLists #-}
module DailyCodingProblem.Problem12Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import DailyCodingProblem.Problem12.Solution
import qualified Data.Set as S

-- | Generate very small integers.
tinyInts :: Gen Int
tinyInts = choose (-10, 25)

-- | Generate a set of very small integers.
stepSizes :: Int -> Gen (S.Set Int)
stepSizes maxSize = S.fromList <$> listOf (choose (1, maxSize))

-- | Combine the two above. Used as input for uniqueClimbsN.
inputs :: Int -> Gen (S.Set Int, Int)
inputs maxSize = (,) <$> stepSizes maxSize <*> (choose (-10, maxSize))

tinyInputs = inputs 20
smallInputs = inputs 100

spec :: Spec
spec = do
    describe "uniqueClimbs" $ do
        prop "gives 0 for negative numbers" $
            \(Negative n) -> uniqueClimbs n `shouldBe` 0

        it "gives 5 when n = 4" $
            uniqueClimbs 4 `shouldBe` 5

    describe "uniqueClimbs'" $
        prop "equal to uniqueClimbs" $ forAll tinyInts $
            \n -> uniqueClimbs' n `shouldBe` uniqueClimbs n
        
    describe "uniqueClimbsN" $ do
        prop "equal to uniqueClimbs when set is {1, 2}" $ forAll tinyInts $
            \n -> uniqueClimbsN [1,2] n `shouldBe` uniqueClimbs n

        it "gives 7 when n = 4 and sizes = {1,2,3}" $
            uniqueClimbsN [1,2,3] 4 `shouldBe` 7
    
    describe "uniqueClimbsN'" $
        prop "equal to uniqueClimbsN" $ forAll tinyInputs $
            \(sizes,n) -> uniqueClimbsN' sizes n `shouldBe` uniqueClimbsN sizes n
        
    describe "uniqueClimbsN''" $
        prop "equal to uniqueClimbsN'" $ forAll smallInputs $
            \(sizes, n) -> uniqueClimbsN'' sizes n `shouldBe` uniqueClimbsN' sizes n