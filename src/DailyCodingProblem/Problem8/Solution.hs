module DailyCodingProblem.Problem8.Solution where

import Data.Maybe (isJust)
import DailyCodingProblem.Utils.BinTree

-- | Get the subtrees of a tree.
subtrees :: BinTree a -> [BinTree a]
subtrees Leaf = []
subtrees t@(Node l _ r) = subtrees l ++ [t] ++ subtrees r

-- | If tree is unival, get the common value. Else Nothing.
univalValue :: Eq a => BinTree a -> Maybe a
univalValue Leaf = Nothing
univalValue (Node l v r) = if eqVal l && eqVal r then Just v else Nothing
    where eqVal t = maybe True (== v) $ univalValue t

-- | Return whether all values in tree are the same.
isUnival :: Eq a => BinTree a -> Bool
isUnival = isJust . univalValue

-- | Get all unival subtrees of a tree.
univalSubtrees :: Eq a => BinTree a -> [BinTree a]
univalSubtrees = filter isUnival . subtrees

-- | Get the number of unival subtrees of a tree.
univalSubtreeCount :: Eq a => BinTree a -> Int
univalSubtreeCount = length . univalSubtrees