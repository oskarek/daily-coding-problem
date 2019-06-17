module DailyCodingProblem.Problem9.Solution where

-- | Group the values into 2-tuples, adding a specified
-- value to the last tuple, if list length is odd.
pairs :: a -> [a] -> [(a, a)]
pairs _ [] = []
pairs b [a] = [(a, b)]
pairs b (x:y:xs) = (x,y) : pairs b xs

-- | Get the largest sum of non-adjacent numbers.
largestNonAdjacentSum :: [Integer] -> Integer
largestNonAdjacentSum = fst . foldr f (0,0) . pairs 0
  where f (x,y) (s1,s2) = (max 0 x + max s1 s2, max 0 y + s2)
