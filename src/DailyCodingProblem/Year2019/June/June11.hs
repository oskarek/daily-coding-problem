{-# LANGUAGE FlexibleInstances #-}
module DailyCodingProblem.Year2019.June.June11 where

import           Text.Megaparsec         hiding ( parse )
import           Data.Maybe                     ( fromJust )
import qualified DailyCodingProblem.Utils.Parsing
                                               as P
import           DailyCodingProblem.Utils.Parsing
                                                ( Parser )

{-
This problem was asked by Google.

Given the root to a binary tree, implement serialize(root), which serializes the tree into a string, and deserialize(s), which deserializes the string back into the tree.

For example, given the following Node class

class Node:
    def __init__(self, val, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

The following test should pass:

node = Node('root', Node('left', Node('left.left')), Node('right'))
assert deserialize(serialize(node)).left.left.val == 'left.left'
-}
data BinTree a =
    Leaf
  | Node (BinTree a) a (BinTree a)
  deriving (Eq, Show)

class Parsable a where parse :: Parser a
instance Parsable a => Parsable (BinTree a) where
    parse = parseTree
instance Parsable Int where
    parse = P.signedInt
instance Parsable String where
    parse = P.stringLiteral

serialize :: Show a => BinTree a -> String
serialize Leaf = "Leaf"
serialize (Node l v r) =
    unwords ["(Node", serialize l, show v, serialize r ++ ")"]

deserialize :: Parsable a => String -> BinTree a
deserialize = fromJust . parseMaybe parse


----- Parsing of BinTrees -------
parseLeaf :: Parser (BinTree a)
parseLeaf = Leaf <$ P.symbol "Leaf"

parseNode :: Parsable a => Parser (BinTree a)
parseNode = do
    _ <- P.symbol "Node"
    l <- parseTree
    v <- P.lexeme parse
    r <- parseTree
    return (Node l v r)

parseTree :: Parsable a => Parser (BinTree a)
parseTree = P.lexeme (P.parens parseNode <|> parseLeaf)


---- Test support for QuickCheck -----
treeFromList :: Ord a => [a] -> BinTree a
treeFromList = foldr insert Leaf

insert :: Ord a => a -> BinTree a -> BinTree a
insert x Leaf = Node Leaf x Leaf
insert x (Node l v r) =
    if x < v then Node (insert x l) v r else Node l v (insert x r)
