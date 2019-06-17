module DailyCodingProblem.Problem3Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import DailyCodingProblem.Problem3.Solution
import DailyCodingProblem.Utils.BinTree

spec :: Spec
spec = do
    describe "serialize & deserialize" $
        prop "deserialize . serialize = id" $
            \tree -> (deserialize . serialize) tree == (tree :: BinTree Int)

    describe "serialize" $ do
        it "handles a leaf correctly" $
            let tree = Leaf :: BinTree Int
             in serialize tree `shouldBe` "Leaf"

        it "handles a simple small tree" $
            let tree = Node (Node Leaf 3 Leaf) 5 (Node Leaf 8 Leaf) :: BinTree Int
             in serialize tree `shouldBe` "(Node (Node Leaf 3 Leaf) 5 (Node Leaf 8 Leaf))"

    describe "deserialize" $ do
        it "handles a leaf correctly" $
            let treeString = "Leaf"
             in deserialize treeString `shouldBe` (Leaf :: BinTree Int)

        it "handles a simple small tree" $
            let treeString = "(Node (Node Leaf 3 Leaf) 5 (Node Leaf 8 Leaf))"
                expectedTree = Node (Node Leaf 3 Leaf) 5 (Node Leaf 8 Leaf) :: BinTree Int
             in deserialize treeString `shouldBe` expectedTree
