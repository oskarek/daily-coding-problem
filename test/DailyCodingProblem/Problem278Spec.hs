module DailyCodingProblem.Problem278Spec (spec) where

import DailyCodingProblem.Problem278.Solution (binTrees)
import Data.Foldable (toList)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Negative (..))

spec :: Spec
spec = describe "binTrees" $ do
  it "gives [] for 0" $
    binTrees 0 `shouldBe` []
  prop "gives [] for negative numbers" $
    \(Negative n) -> binTrees n `shouldBe` []
  it "gives trees representing [1 .. n]" $
    mapM_ (\n -> mapM_ (shouldBe [1 .. n]) $ toList <$> binTrees n) [1 .. 10]
