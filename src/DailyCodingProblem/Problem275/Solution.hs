module DailyCodingProblem.Problem275.Solution where

import qualified Data.List.NonEmpty as NE

nextLookAndSayNum :: String -> String
nextLookAndSayNum prev = f `foldMap` NE.group prev
  where
    f str = show (length str) ++ [NE.head str]

lookAndSaySeq :: [String]
lookAndSaySeq = iterate nextLookAndSayNum "1"

nthLookAndSayNumber :: Int -> Integer
nthLookAndSayNumber n = read (lookAndSaySeq !! n)