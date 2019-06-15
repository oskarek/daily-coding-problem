module DailyCodingProblem.Problem5Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import DailyCodingProblem.Problem5.Solution

spec :: Spec
spec =
    describe "solution" $ do
        prop "car(cons(a,b)) = a" $
            \a b -> car (cons (a :: Int) (b :: Int)) == a

        prop "cdr(cons(a,b)) = b" $
            \a b -> cdr (cons (a :: Int) (b :: Int)) == b
