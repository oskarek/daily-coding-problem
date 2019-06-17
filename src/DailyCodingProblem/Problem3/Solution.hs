{-# LANGUAGE FlexibleInstances #-}
module DailyCodingProblem.Problem3.Solution where

import           Text.Megaparsec         hiding ( parse )
import           Data.Maybe                     ( fromJust )
import qualified DailyCodingProblem.Utils.Parsing
                                               as P
import           DailyCodingProblem.Utils.Parsing
                                                ( Parser )
import DailyCodingProblem.Utils.BinTree

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
