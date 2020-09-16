module DailyCodingProblem.Problem281.Solution where

import Data.List (group, maximumBy, sort)
import Data.Ord (comparing)

fewestNumberOfBricks :: [[Int]] -> Int
fewestNumberOfBricks =
  mostFrequent . concatMap (init . scanl1 (+))

mostFrequent :: Ord a => [a] -> a
mostFrequent = head . maximumBy (comparing length) . group . sort
