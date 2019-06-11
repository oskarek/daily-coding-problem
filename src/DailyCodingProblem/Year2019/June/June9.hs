module DailyCodingProblem.Year2019.June.June9 where

import           Data.List                      ( tails )

{-
This problem was recently asked by Google.

Given a list of numbers and a number k, return whether any two
numbers from the list add up to k.

For example, given [10, 15, 3, 7] and k of 17, return true since
10 + 7 is 17.

Bonus: Can you do this in one pass?
-}
twoAddsUpToK :: Integer -> [Integer] -> Bool
twoAddsUpToK k xs = k `elem` [ x + y | (x : xs') <- tails xs, y <- xs' ]
