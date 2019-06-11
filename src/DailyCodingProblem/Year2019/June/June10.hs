module DailyCodingProblem.Year2019.June.June10 where

import           Data.Monoid

{-
This problem was asked by Uber.

Given an array of integers, return a new array such that each element
at index i of the new array is the product of all the numbers in the
original array except the one at i.

For example, if our input was [1, 2, 3, 4, 5], the expected output
would be [120, 60, 40, 30, 24]. If our input was [3, 2, 1], the
expected output would be [2, 3, 6].

Follow-up: what if you can't use division?
-}
productArray :: [Integer] -> [Integer]
productArray = map getProduct . mArray . map Product

mArray :: Monoid m => [m] -> [m]
mArray = go mempty
  where
    go _   []       = []
    go acc (x : xs) = (acc <> mconcat xs) : go (acc <> x) xs

{-
Bonus 1:

Given an array of integers, return a new array such that each element
at index i of the new array is the SUM of all the numbers in the
original array except the one at i.

Thanks to generalization mArray, this is super easy!
-}
sumArray :: [Integer] -> [Integer]
sumArray = map getSum . mArray . map Sum

{-
Bonus 2:

Given an array of ARRAYS OF integers, return a new array such that each element
at index i of the new array is the CONCATENATION of all the numbers in the
original array except the one at i.

Thanks to generalization mArray, this is super easy!
-}
concatArray :: [[Integer]] -> [[Integer]]
concatArray = mArray
