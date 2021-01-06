module DailyCodingProblem.Problem294Spec (spec) where

import DailyCodingProblem.Problem294.Solution (shortestRunPath)
import qualified Data.Map as Map
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "shortestRunPath" $
    it "handles the sample input" $
      let elevations = Map.fromList [(0, 5), (1, 25), (2, 15), (3, 20), (4, 10)]
          paths =
            Map.fromList
              [ (0, [(1, 10), (2, 8), (3, 15)]),
                (1, [(3, 12)]),
                (2, [(4, 10)]),
                (3, [(4, 5), (0, 17)]),
                (4, [(0, 10)])
              ]
       in shortestRunPath elevations paths `shouldBe` Just 28
