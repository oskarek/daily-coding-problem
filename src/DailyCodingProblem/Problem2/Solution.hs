module DailyCodingProblem.Problem2.Solution where

import           Data.Monoid

productArray :: [Integer] -> [Integer]
productArray = map getProduct . mArray . map Product

mArray :: Monoid m => [m] -> [m]
mArray = go mempty
  where
    go _   []       = []
    go acc (x : xs) = (acc <> mconcat xs) : go (acc <> x) xs

{-
Bonus 1:

Use SUM of integers instead of product.

Thanks to the generalization mArray, this is super easy!
-}
sumArray :: [Integer] -> [Integer]
sumArray = map getSum . mArray . map Sum

{-
Bonus 2:

Use CONCATENATION of ARRAYS of integers instead of integer product.

Also super easy, thanks to mArray.
-}
concatArray :: [[Integer]] -> [[Integer]]
concatArray = mArray
