module DailyCodingProblem.Problem294.Solution where

import Data.Function (on)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import Safe (minimumMay)

data PathState = Start | Up | Down deriving (Eq, Show)

shortestRunPath :: Map.Map Int Int -> Map.Map Int [(Int, Int)] -> Maybe Int
shortestRunPath = go Start 0 0
  where
    go state start end elevations paths
      | start == end && state == Up = Nothing
      | start == end && state == Down = Just 0
      | otherwise =
        let neighbsBy comp =
              filter ((comp `on` (elevations Map.!)) start . fst) (paths Map.! start)
            minBy st comp =
              minimumMay $ (\(id, dist) -> (+ dist) <$> go st id end elevations paths) `mapMaybe` neighbsBy comp
         in case state of
              Start -> minBy Up (<=)
              Up -> minimumMay $ catMaybes [minBy Up (<=), minBy Down (>=)]
              Down -> minBy Down (>=)
