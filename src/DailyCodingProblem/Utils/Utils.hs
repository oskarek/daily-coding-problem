module DailyCodingProblem.Utils.Utils where

import Data.Functor.Identity

-- | Fold from the left within a monad as long as possible,
-- until next accumulated value would break predicate.
foldMWhile :: (Foldable t, Monad m)
              => (b -> Bool)
              -> (b -> a -> m b) -> b -> t a -> m b
foldMWhile pred reducer acc list = foldr cons return list acc
  where
    cons x r acc = do
      next <- reducer acc x
      if not (pred next)
        then return acc
        else r $! next

-- | Fold from the left as long as possible, until
-- next accumulated value would break predicate.
foldlWhile :: Foldable t
              => (b -> Bool)
              -> (b -> a -> b) -> b -> t a -> b
foldlWhile pred reducer acc =
  runIdentity . foldMWhile pred (\b a -> Identity $ reducer b a) acc

-- | The number of elements satisfying the predicate.
count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

-- | The number of occurrences of the specified element in the list.
occurrences :: Eq a => a -> [a] -> Int
occurrences x = count (== x)
