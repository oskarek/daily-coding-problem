{-# LANGUAGE OverloadedLists #-}
module DailyCodingProblem.Problem7Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import DailyCodingProblem.Problem7.Solution

newtype Digit = Digit Char deriving (Eq, Show)
instance Arbitrary Digit where
    arbitrary = Digit <$> choose ('0', '9')

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

        it "doesn't allow '01' as a message" $
            numberOfDecodings "01" `shouldBe` 0

        it "gives 3 as result for message '111'" $
            numberOfDecodings "111" `shouldBe` 3

    describe "numberOfDecodings and numberOfDecodings'" $
        prop "they are equal" $
            \message ->
                let message' = (\(Digit d) -> d) <$> message
                in  numberOfDecodings message' == numberOfDecodings' message'

    describe "numberOfDecodings and numberOfDecodings''" $
        prop "they are equal" $
            \message ->
                let message' = (\(Digit d) -> d) <$> message
                in  numberOfDecodings message' == numberOfDecodings'' message'