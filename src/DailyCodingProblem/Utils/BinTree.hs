module DailyCodingProblem.Utils.BinTree where

import Test.QuickCheck

data BinTree a =
    Leaf
    | Node (BinTree a) a (BinTree a)
    deriving (Eq, Show)

instance (Arbitrary a, Ord a) => Arbitrary (BinTree a) where
    arbitrary = fromList <$> listGen arbitrary
      where
        listGen gen = sized $ \n ->
            do k <- choose (0,n)
               vectorOf k gen

instance Functor BinTree where
    fmap _ Leaf = Leaf
    fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

instance Foldable BinTree where
    foldMap _ Leaf = mempty
    foldMap f (Node l v r) = foldMap f l <> f v <> foldMap f r

fromList :: Ord a => [a] -> BinTree a
fromList = foldr insert Leaf

insert :: Ord a => a -> BinTree a -> BinTree a
insert x Leaf = Node Leaf x Leaf
insert x (Node l v r) =
    if x < v then Node (insert x l) v r else Node l v (insert x r)
