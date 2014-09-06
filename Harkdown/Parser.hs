module Harkdown.Parser ( Harkdown(..), parser ) where

import Control.Applicative hiding ( many, (<|>) )
import Text.ParserCombinators.Parsec

data Harkdown = Paragraph String
              | List [String]
              deriving Show

listItem = string "- " *> many (noneOf "\n") <* char '\n'

list = List <$> many1 listItem

paragraph = Paragraph <$> many (noneOf "\n") <* char '\n'

parser = list <|> paragraph
