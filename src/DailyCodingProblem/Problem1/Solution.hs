module DailyCodingProblem.Problem1.Solution where

import           Data.List                      ( tails )

twoAddsUpToK :: Integer -> [Integer] -> Bool
twoAddsUpToK k xs = k `elem` [ x + y | (x : xs') <- tails xs, y <- xs' ]
