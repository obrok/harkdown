module Harkdown.Parser ( ParagraphContent(..), Harkdown(..), parser ) where

import Control.Applicative hiding ( many, (<|>), optional )
import Text.ParserCombinators.Parsec
import Harkdown.Tools

data ParagraphContent = Text String
                      | Emphasis String
                      deriving Show

data Harkdown = Paragraph [ParagraphContent]
              | ListItem String
              | List [Harkdown]
              | HorizontalLine
              | Sequence [Harkdown]
              | CodeBlock String
              deriving Show

whitespace = many (char ' ')

untill c = manyTill anyToken (char c)

concreteHorizontalRule ruleMarker = HorizontalLine <$ try (
  whitespace *> string ruleMarker *>
  whitespace *> string ruleMarker *>
  whitespace *> string ruleMarker *>
  many (whitespace *> string ruleMarker) *> char '\n')

horizontalRule = concreteHorizontalRule "*" <|> concreteHorizontalRule "_" <|> concreteHorizontalRule "-"

listItem = ListItem <$> (try (string "- ") *> many (noneOf "\n") <* char '\n')

list = List <$> many1 listItem

paragraphText = Text <$> (many1 (noneOf "\n") <* char '\n')

emphasis = Emphasis <$> try (whitespace *> between (string "*") (string "*") (many1 $ noneOf "*"))

paragraphContent = emphasis <|> paragraphText

emptyLine = char '\n'

paragraph = Paragraph <$> (many1 paragraphContent <* optional emptyLine)

codeBlock = CodeBlock <$> (try (string "    ") *> many (noneOf "\n") <* string "\n")

parser = Sequence <$> many (codeBlock <|> horizontalRule <|> list <|> paragraph)
