module Harkdown.Parser ( Harkdown(..), parser ) where

import Control.Applicative hiding ( many, (<|>) )
import Text.ParserCombinators.Parsec
import Harkdown.Tools

type ParagraphLine = String

data Harkdown = Paragraph [ParagraphLine]
              | ListItem String
              | List [Harkdown]
              | HorizontalLine
              | Sequence [Harkdown]
              | CodeBlock String
              deriving Show

horizontalLine = HorizontalLine <$
  try (many (char ' ')
  *> (string "***" <|> string "---" <|> string "___")
  *> manyTill anyToken (char '\n'))

listItem = ListItem <$> (try (string "- ") *> many (noneOf "\n") <* char '\n')

list = List <$> many1 listItem

paragraphLine = many (noneOf "\n") <* char '\n'

paragraph = Paragraph <$> many1 paragraphLine

codeBlock = CodeBlock <$> (try (string "    ") *> many (noneOf "\n") <* string "\n")

parser = Sequence <$> many (codeBlock <|> horizontalLine <|> list <|> paragraph)
