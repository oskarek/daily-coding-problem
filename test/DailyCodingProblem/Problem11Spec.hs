module DailyCodingProblem.Problem11Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import DailyCodingProblem.Problem11.Solution
import qualified Data.Set as S
import qualified Data.List as L

spec :: Spec
spec =
    describe "autocomplete" $
        prop "always returns every word that has the given string as prefix" $
            \s ss -> autocomplete s (fromList ss)
                `shouldBe` S.filter (s `L.isPrefixOf`) (S.fromList ss)
