{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module DailyCodingProblem.Problem13.Solution where

import qualified Data.Set as S
import DailyCodingProblem.Utils.Utils (foldlWhile, foldMWhile)
import qualified Data.Vector as V
import Control.Monad.State
import Control.Lens
import qualified Data.IntMap as IM
import Data.Char (ord)

-- | Return suffixes of all possible lengths of a list.
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes (x:xs) = (x:xs) : suffixes xs

-- | O(n*log n) Given an integer k and a string s, return the length
-- of the longest prefix of s that contains at most k
-- distinct characters.
longestKSPrefixLength :: Int -> String -> Int
longestKSPrefixLength k = fst
    . foldlWhile ((<= k) . S.size . snd) ins (0,S.empty)
    where ins (n,s) c = (n+1, S.insert c s)

-- | O(n^2*log n) - Given an integer k and a string s,
-- return the length of the longest substring of s that
-- contains at most k distinct characters.
longestKSLength :: Int -> String -> Int
longestKSLength k =
    maximum . fmap (longestKSPrefixLength k) . suffixes

-------- FASTER SOLUTION --------
data WindowState = WS
  { _start :: Int
  , _end :: Int
  , _lastIdx :: IM.IntMap Int
  , _kChars :: S.Set Char
  , _longest :: Int } deriving (Eq, Show)

makeLenses ''WindowState

slideWindow :: Int -> V.Vector Char -> State WindowState ()
slideWindow k cs = get >>= \st -> do
  let firstChar = cs V.! (st^.start)

  (end', kChars') <- foldMWhile ((<= k) . S.size . snd)
                                ins
                                (st^.end, st^.kChars)
                                [st^.end .. V.length cs - 1]

  modify $ \s -> s
    & start .~ ((s^.lastIdx) IM.! ord firstChar) + 1
    & end .~ end'
    & kChars .~ S.delete firstChar kChars'
    & longest %~ max (end' - s^.start)

  where
    ins (n,s) i = do
      modify (lastIdx %~ IM.insert (ord $ cs V.! i) i)
      return (n+1, S.insert (cs V.! i) s)


longestWithState :: Int -> V.Vector Char -> State WindowState ()
longestWithState _ cs | V.null cs = return ()
longestWithState k cs = get >>= \st -> when (st^.end < V.length cs) $ do
  slideWindow k cs
  longestWithState k cs

-- | O(n*log k) - A much more efficient version of longestKSLength that
-- uses a sliding window strategy.
longestKSLength' :: Int -> V.Vector Char -> Int
longestKSLength' k cs =
  view longest $ execState (longestWithState k cs) (WS 0 0 IM.empty S.empty 0)