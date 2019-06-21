module DailyCodingProblem.Problem12.Solution where

import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Vector ((!))

-- | Naive solution
-- Idea: When there are n steps left to take, you have two options:
--   - either you take 2 steps at a time, in which case you have (n-2)
--     steps left to take. From there there are `uniqueClimbs (n-2)`
--     unique ways to get to the top
--   - or you take 1 step at a time, in which case there are
--     `uniqueClimbs (n-1)` unique ways to get to the top.
--
-- Since these two are the only options, the total number of ways are
-- their sum.
--
-- Note: We see that this turns out to be, for non-negative numbers,
-- exactly the definition of the fibonacci function.
--
-- Time complexity is not great. O(2^n).
uniqueClimbs :: Int -> Int
uniqueClimbs n | n < 0 = 0
uniqueClimbs 0 = 1
uniqueClimbs 1 = 1
uniqueClimbs n = uniqueClimbs (n-2) + uniqueClimbs (n-1)

-- | O(n) - Much more efficient solution, using dynamic programming.
uniqueClimbs' :: Int -> Int
uniqueClimbs' n | n < 0 = 0
uniqueClimbs' n = snd $ foldl f (1,1) [2 .. n]
    where f (s1, s2) _ = (s2, s1+s2)


-- | O(|X|^n) - A generalized version of uniqueClimbs, in which we are given a
-- set X of all the possible numbers of steps that can be taken at a time.
uniqueClimbsN :: S.Set Int -> Int -> Int
uniqueClimbsN sizes n | n < 0 || null sizes = 0
uniqueClimbsN _ 0 = 1
uniqueClimbsN _ 1 = 1
uniqueClimbsN sizes n = sum $ uniqueClimbsN sizes . (n -) <$> S.toList sizes

-- | O(n*|X|*xmax) where xmax = maximum value of X
-- A more efficient solution, using dynamic programming.
--
-- Not optimal in terms of speed, however space complexity is
-- O(xmax) which is better than uniqueClimbsN''.
uniqueClimbsN' :: S.Set Int -> Int -> Int
uniqueClimbsN' sizes n | n < 0 || null sizes = 0
uniqueClimbsN' sizes n = head $ foldl f (1 : 1 : replicate (m-2) 0) [2 .. n]
  where
    m = S.findMax sizes
    f p _ = sum ((\n -> p !! (n-1)) <$> S.toList sizes) : init p

-- | O(n*|X|) - An ever more efficient version, in terms of speed.
-- However it has O(n) space complexity, which is worse than uniqueClimbsN'.
uniqueClimbsN'' :: S.Set Int -> Int -> Int
uniqueClimbsN'' sizes n | n < 0 || null sizes = 0
uniqueClimbsN'' sizes n = (! n) r
  where
    r = V.generate (n+1) gen
    gen i | i == 0 || i == 1 = 1
    gen i =
      let idxs = filter (<= i) (S.toList sizes)
      in sum $ map (\idx -> r ! (i-idx)) idxs