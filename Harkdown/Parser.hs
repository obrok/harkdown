module Harkdown.Parser ( Harkdown(..), parser ) where

import Text.ParserCombinators.Parsec
import Control.Monad

data Harkdown = Paragraph String
              | List [String]
              deriving Show

listItem = do
  string "- "
  result <- many (noneOf "\n")
  char '\n'
  return result

list = do
  result <- many1 listItem
  return $ List result

paragraph = do
  result <- many (noneOf "\n")
  char '\n'
  return $ Paragraph result

parser = list <|> paragraph
