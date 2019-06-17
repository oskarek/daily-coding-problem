module DailyCodingProblem.Problem8.Solution where

import DailyCodingProblem.Utils.BinTree

-- | Get the subtrees of a tree.
subtrees :: BinTree a -> [BinTree a]
subtrees Leaf = []
subtrees t@(Node l _ r) = subtrees l ++ [t] ++ subtrees r

-- | Get the root value of the tree, if one exists.
rootValue :: BinTree a -> Maybe a
rootValue Leaf = Nothing
rootValue (Node _ v _) = Just v

-- | Return whether all values in tree are the same.
isUnival :: Eq a => BinTree a -> Bool
isUnival Leaf = True
isUnival (Node l v r) = and [isUnival l, eqVal l, isUnival r, eqVal r]
    where eqVal = maybe True (== v) . rootValue

-- | Get all unival subtrees of a tree.
univalSubtrees :: Eq a => BinTree a -> [BinTree a]
univalSubtrees = filter isUnival . subtrees

-- | Get the number of unival subtrees of a tree.
univalSubtreeCount :: Eq a => BinTree a -> Int
univalSubtreeCount = length . univalSubtrees