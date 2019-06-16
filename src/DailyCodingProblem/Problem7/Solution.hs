
module DailyCodingProblem.Problem7.Solution where

import qualified Data.Set                      as S
import qualified Data.Map                      as M
import           Data.Ix                        ( inRange )
import qualified Data.Char                     as Ch

type DecodedMessage = String
type EncodedMessage = String

digitToInt :: Char -> Maybe Int
digitToInt d = if Ch.isDigit d then Just (Ch.digitToInt d) else Nothing

-- | Get the number of possible decodings of a message.
numberOfDecodings :: EncodedMessage -> Int
numberOfDecodings = maybe 0 go . traverse digitToInt
  where
    go []  = 1
    go [x] = if x == 0 then 0 else 1
    go (x : y : xs) =
      let path1 = if x <= 0 then 0 else go (y : xs)
          path2 = if inRange (1,26) (10*x + y) then go xs else 0
      in  path1 + path2


---- BONUS: Solution to also get the actual decodings ----

mapping :: M.Map Int Char
mapping = M.fromAscList (zip [1 ..] ['a' .. 'z'])

-- | Get the possible decodings of a message.
possibleDecodings :: EncodedMessage -> S.Set DecodedMessage
possibleDecodings = maybe S.empty go . traverse digitToInt
  where
    go []           = S.singleton ""
    go [x]          = maybe S.empty (S.singleton . (:[])) (mapping M.!? x)
    go (x : y : xs) = path1 <> path2
      where
        path1 = case mapping M.!? x of
          Nothing -> S.empty
          Just c -> (c :) `S.map` go (y : xs)
        path2 = case mapping M.!? (10*x + y) of
          Nothing -> S.empty
          Just c  -> (c :) `S.map` go xs

-- Now numberOfDecodings can be expressed in terms of possibleDecodings
numberOfDecodings' :: EncodedMessage -> Int
numberOfDecodings' = length . possibleDecodings
