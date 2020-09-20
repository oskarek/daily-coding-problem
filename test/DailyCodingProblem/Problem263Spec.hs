module DailyCodingProblem.Problem263Spec (spec) where

import DailyCodingProblem.Problem263.Solution
import Test.Hspec

spec :: Spec
spec = describe "isValidSentence" $
  it "handles some sample input" $ do
    isValidSentence "" `shouldBe` False
    isValidSentence "H" `shouldBe` False
    isValidSentence "H." `shouldBe` True
    isValidSentence "Hej, vad gör du?" `shouldBe` True
    isValidSentence "hej, vad gör du?" `shouldBe` False
    isValidSentence "Hej,  vad gör du?" `shouldBe` False
    isValidSentence "Hej, var, gör; du!" `shouldBe` True
    isValidSentence "Hej,, var, gör; du!" `shouldBe` False
    isValidSentence "Hej, vad gör du??" `shouldBe` False
    isValidSentence ".Hej, vad gör du?" `shouldBe` False
    isValidSentence "Hej, vad gör Du?" `shouldBe` False
