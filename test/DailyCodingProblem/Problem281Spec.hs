module DailyCodingProblem.Problem281Spec (spec) where

import DailyCodingProblem.Problem281.Solution
import Test.Hspec

spec :: Spec
spec =
  describe "fewestNumberOfBricks" $
    it "handles the sample input" $
      fewestNumberOfBricks
        [ [3, 5, 1, 1],
          [2, 3, 3, 2],
          [5, 5],
          [4, 4, 2],
          [1, 3, 3, 3],
          [1, 1, 6, 1, 1]
        ]
        `shouldBe` 8
