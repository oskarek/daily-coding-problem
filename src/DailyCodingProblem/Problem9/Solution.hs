module DailyCodingProblem.Problem9.Solution where

-- | Get the largest sum of non-adjacent numbers.
largestNonAdjacentSum :: [Integer] -> Integer
largestNonAdjacentSum = snd . foldl f (0,0)
  where f (s1,s2) x = (s2, max s2 (max 0 x + s1))