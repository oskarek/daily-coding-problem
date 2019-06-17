module DailyCodingProblem.Problem6Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import DailyCodingProblem.Problem6.Solution
import Control.Monad

spec :: Spec
spec =
    describe "XORLinkedList" $ do
        describe "add and get" $ do
            it "handles a simple flow of adding and getting data" $
                let
                    vals = runAlloc $ do
                        let ll = mkLinkedList :: XORLinkedList String
                        ll1 <- add ll "a"
                        ll2 <- add ll1 "b"
                        ll3 <- add ll2 "c"
                        a <- ll3 ! 0
                        b <- ll3 ! 1
                        c <- ll3 ! 2
                        return (a, b, c)
                in
                    vals `shouldBe` (Just "a", Just "b", Just "c")

            it "returns Nothing when index is out of bounds" $
                let
                    val = runAlloc $ do
                        let ll = mkLinkedList :: XORLinkedList String
                        ll1 <- add ll "a"
                        ll1 ! 1
                in
                    val `shouldBe` Nothing

        describe "add" $
            prop "can add any number of items" $
                \xs ->
                    let
                        vals = runAlloc $ do
                            ll <- foldM add mkLinkedList xs
                            let indices = fst <$> zip [0..] xs
                            traverse (ll !) indices
                    in
                        vals `shouldBe` (Just <$> xs :: [Maybe Int])
