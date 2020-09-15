module DailyCodingProblem.Problem275Spec (spec) where

import DailyCodingProblem.Problem275.Solution
import Test.Hspec

spec :: Spec
spec = do
  describe "nextLookAndSayNum" $ do
    it "returns '11' for '1'" $
      nextLookAndSayNum "1" `shouldBe` "11"
    it "returns '31221331321113' for '11122311122213'" $
      nextLookAndSayNum "11122311122213" `shouldBe` "31221331321113"

  describe "lookAndSaySeq" $
    it "starts correctly" $
      take 5 lookAndSaySeq `shouldBe` ["1", "11", "21", "1211", "111221"]

  describe "nthLookAndSayNumber" $
    it "is 0-indexed" $
      nthLookAndSayNumber 0 `shouldBe` 1
