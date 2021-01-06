module DailyCodingProblem.Problem285Spec (spec) where

import DailyCodingProblem.Problem285.Solution (sunSettingCount)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "sunSettingCount" $
    it "handles the sample input" $
      sunSettingCount [3, 7, 8, 3, 6, 1] `shouldBe` 3
