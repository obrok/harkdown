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
              deriving Show

horizontalLine = HorizontalLine <$ try (string "***\n" <|> string "---\n" <|> string "___\n")

listItem = ListItem <$> (try (string "- ") *> many (noneOf "\n") <* char '\n')

list = List <$> many1 listItem

paragraphLine = many (noneOf "\n") <* char '\n'

paragraph = Paragraph <$> many1 paragraphLine

parser = Sequence <$> many (horizontalLine <|> list <|> paragraph)
