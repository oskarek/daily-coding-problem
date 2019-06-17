module DailyCodingProblem.Problem8Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import DailyCodingProblem.Problem8.Solution
import DailyCodingProblem.Utils.BinTree

tree :: BinTree Int
tree =
    Node
        (Node Leaf 1 Leaf)
        0
        (Node
            (Node
                (Node Leaf 1 Leaf)
                1
                (Node Leaf 1 Leaf)
            )
            0 
            (Node Leaf 0 Leaf)
        ) :: BinTree Int

spec :: Spec
spec = do
    describe "subtrees" $ do
        it "handles Leaf" $
            subtrees (Leaf :: BinTree Int) `shouldBe` []
        
        it "handles a simple tree" $
            let expected =
                    [ Node Leaf 1 Leaf
                    , tree 
                    , Node Leaf 1 Leaf
                    , Node
                        (Node Leaf 1 Leaf)
                        1
                        (Node Leaf 1 Leaf)
                    , Node Leaf 1 Leaf
                    , Node
                        (Node
                            (Node Leaf 1 Leaf)
                            1
                            (Node Leaf 1 Leaf)
                        )
                        0 
                        (Node Leaf 0 Leaf)
                    , Node Leaf 0 Leaf ]
            in subtrees tree `shouldBe` expected
    
    describe "univalSubtreeCount" $ do
        it "gives 0 for an empty tree" $
            univalSubtreeCount (Leaf :: BinTree Int) `shouldBe` 0
        
        it "gives 5 for the example tree" $
            univalSubtreeCount tree `shouldBe` 5

    describe "isUnival" $ do
        prop "returns False when at least two values in tree differ" $
            \xs xs' xs'' v -> let tree' = fromList (xs ++ [v] ++ xs' ++ [v+1] ++ xs'') :: BinTree Int
                              in  isUnival tree' `shouldBe` False

        prop "return True when all values are the same" $
            \n -> let tree' = fromList (replicate n 0) :: BinTree Int
                   in isUnival tree' `shouldBe` True
