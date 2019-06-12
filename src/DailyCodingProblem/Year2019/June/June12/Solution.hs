module DailyCodingProblem.Year2019.June.June12.Solution where

import qualified Data.List                     as L
import qualified Data.Maybe                    as M
import qualified Data.IntSet                   as IS

-- | Return smallest positive integer not in list.
-- |
-- | Linear time complexity thanks to IntSet
firstMissingPosInt :: [Int] -> Int
firstMissingPosInt xs =
    let set = IS.fromList xs
     in M.fromJust $ L.find (`IS.notMember` set) [1 ..]
