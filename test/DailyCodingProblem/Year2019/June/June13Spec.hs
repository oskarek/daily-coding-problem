module DailyCodingProblem.Year2019.June.June13Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import DailyCodingProblem.Year2019.June.June13.Solution

spec :: Spec
spec =
    describe "solution" $ do
        prop "car(cons(a,b)) = a" $
            \a b -> car (cons (a :: Int) (b :: Int)) == a

        prop "cdr(cons(a,b)) = b" $
            \a b -> cdr (cons (a :: Int) (b :: Int)) == b
