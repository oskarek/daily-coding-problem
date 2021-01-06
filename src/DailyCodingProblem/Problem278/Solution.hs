module DailyCodingProblem.Problem278.Solution where

import DailyCodingProblem.Utils.BinTree

binTrees :: Int -> [BinTree Int]
binTrees = go 1
  where
    go low high = do
      k <- [low .. high]
      l <- fromEmpty Leaf (go low (k - 1))
      r <- fromEmpty Leaf (go (k + 1) high)
      return (Node l k r)

fromEmpty :: a -> [a] -> [a]
fromEmpty x [] = [x]
fromEmpty _ xs = xs
