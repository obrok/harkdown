module Harkdown.Parser ( ParagraphContent(..), Harkdown(..), parser ) where

import Control.Applicative hiding ( many, (<|>), optional )
import Text.ParserCombinators.Parsec hiding ( space, newline )
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
              | Header String
              deriving Show

newline = char '\n'
space = char ' '

whitespace = many space

untill c = manyTill anyToken (char c)

horizontalRuleOf ruleMarker = HorizontalLine <$ try (
  optional space *> optional space *> optional space *> string ruleMarker *>
  whitespace *> string ruleMarker *>
  whitespace *> string ruleMarker *>
  many (whitespace *> string ruleMarker) *> char '\n')

horizontalRule = horizontalRuleOf "*" <|> horizontalRuleOf "_" <|> horizontalRuleOf "-"

listItem = ListItem <$> (try (string "- ") *> many (noneOf "\n") <* char '\n')

list = List <$> many1 listItem

paragraphText = Text <$> (many1 (noneOf "\n") <* char '\n')

emphasis = Emphasis <$> try (whitespace *> between (string "*") (string "*") (many1 $ noneOf "*"))

setextHeader = Header <$> try (manyTill anyToken newline <* string "---\n")

paragraphContent = notFollowedBy horizontalRule *> (emphasis <|> paragraphText)

emptyLine = char '\n'

paragraph = Paragraph <$> (many1 paragraphContent <* optional emptyLine)

codeBlock = CodeBlock <$> (try (string "    ") *> many (noneOf "\n") <* string "\n")

parser = Sequence <$> many (codeBlock <|> horizontalRule <|> setextHeader <|> list <|> paragraph)
