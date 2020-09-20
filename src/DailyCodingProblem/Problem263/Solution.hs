module DailyCodingProblem.Problem263.Solution where

import DailyCodingProblem.Utils.Parsing (Parser)
import Data.Maybe (isJust)
import Text.Megaparsec
import Text.Megaparsec.Char

word :: Parser String
word = some lowerChar

startWord :: Parser String
startWord = (:) <$> upperChar <*> many lowerChar

withOptSeparator :: Parser String -> Parser String
withOptSeparator word = strings [word, separator <|> return ""]

separator :: Parser String
separator = (: []) <$> oneOf ",;:"

terminalMark :: Parser String
terminalMark = (: []) <$> oneOf ".?!‽"

strings :: [Parser String] -> Parser String
strings = fmap mconcat . sequence

sentence :: Parser String
sentence =
  strings
    [ withOptSeparator startWord,
      unwords <$> many (strings [string " ", withOptSeparator word]),
      terminalMark
    ]

isValidSentence :: String -> Bool
isValidSentence = isJust . parseMaybe (sentence <* eof)