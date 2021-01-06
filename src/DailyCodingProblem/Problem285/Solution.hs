{-# LANGUAGE ViewPatterns #-}

module DailyCodingProblem.Problem285.Solution where

import DailyCodingProblem.Utils.Utils (count)

sunSettingCount :: [Int] -> Int
sunSettingCount (reverse -> heights) =
  let blockingHeights = scanl max 0 heights
   in count (uncurry (>)) $ zip heights blockingHeights