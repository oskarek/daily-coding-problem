module DailyCodingProblem.Problem11.Solution where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M

----- First a naive solution ------

-- | Given a string and a dictionary of words, return all words
-- with the string as prefix.
--
-- O(n) complexity where n is the number of words in the dictionary.
autocomplete_naive :: String -> S.Set String -> S.Set String
autocomplete_naive s = S.filter (s `L.isPrefixOf`)

----- A more sophisticated solution using a trie data structure -----
data Trie a = Node Bool (M.Map a (Trie a)) deriving (Show, Eq)

-- | Insert an element into the trie.
insert :: Ord a => [a] -> Trie a -> Trie a
insert [] (Node _ m) = Node True m
insert (x:xs) (Node b m) = if x `M.member` m
    then Node b $ M.adjust (xs `insert`) x m
    else Node b (M.insert x (insert xs (Node False M.empty)) m)

-- | Is the element in the trie? (Bonus function; not actually used here.)
member :: Ord a => [a] -> Trie a -> Bool
member []     (Node b _) = b
member (x:xs) (Node _ m) = maybe False (xs `member`) (m M.!? x)

-- | Return all elements of the trie, prepending the given list to all.
elemsPrepend :: Ord a => [a] -> Trie a -> [[a]]
elemsPrepend acc (Node b m) = [reverse acc | b]
    ++ concatMap (\(a, t) -> elemsPrepend (a:acc) t) (M.toList m)

-- | Return all elements of the trie.
elems :: Ord a => Trie a -> [[a]]
elems = elemsPrepend []

-- | Return all elements of the trie that starts with the given prefix.
elemsWithPrefix :: Ord a => [a] -> Trie a -> [[a]]
elemsWithPrefix = go []
  where
    go acc []     t          = elemsPrepend (reverse acc) t
    go acc (x:xs) (Node _ m) = maybe [] (go (x:acc) xs) (m M.!? x)

-- | Convert a list to a trie.
fromList :: Ord a => [[a]] -> Trie a
fromList = foldr insert (Node False M.empty)

-- | Given a string and a dictionary of words, return all words
-- with the string as prefix.
--
-- Still O(n) worst case, but with a larger prefix, the runtime
-- generally decreases significantly.
autocomplete :: String -> Trie Char -> S.Set String
autocomplete s = S.fromList . elemsWithPrefix s