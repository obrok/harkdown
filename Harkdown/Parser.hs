module Harkdown.Parser ( Harkdown(..), parser ) where

import Control.Applicative hiding ( many, (<|>), optional )
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

whitespace = many (char ' ')

horizontalLineChar = string "*" <|> string "-" <|> string "_"

untill c = manyTill anyToken (char c)

horizontalLine = HorizontalLine <$ try (
  whitespace *> horizontalLineChar *>
  whitespace *> horizontalLineChar *>
  whitespace *> horizontalLineChar *>
  many (whitespace *> horizontalLineChar) *> char '\n')

listItem = ListItem <$> (try (string "- ") *> many (noneOf "\n") <* char '\n')

list = List <$> many1 listItem

paragraphLine = many1 (noneOf "\n") <* char '\n'

emptyLine = char '\n'

paragraph = Paragraph <$> (many1 paragraphLine <* optional emptyLine)

codeBlock = CodeBlock <$> (try (string "    ") *> many (noneOf "\n") <* string "\n")

parser = Sequence <$> many (codeBlock <|> horizontalLine <|> list <|> paragraph)
