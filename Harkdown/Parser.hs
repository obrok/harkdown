module Harkdown.Parser ( Harkdown(..), parser ) where

import Control.Applicative hiding ( many, (<|>) )
import Text.ParserCombinators.Parsec

data Harkdown = Paragraph String
              | ListItem String
              | List [Harkdown]
              | HorizontalLine
              | Sequence [Harkdown]
              deriving Show

horizontalLine = try (string "***\n" <|> string "---\n" <|> string "___\n") *> return HorizontalLine

listItem = ListItem <$> (string "- " *> many (noneOf "\n") <* char '\n')

list = List <$> many1 listItem

paragraph = Paragraph <$> many (noneOf "\n") <* char '\n'

parser = Sequence <$> many (horizontalLine <|> list <|> paragraph)
