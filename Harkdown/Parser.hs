module Harkdown.Parser ( ParagraphContent(..), Harkdown(..), parser ) where

import Control.Applicative hiding ( many, (<|>), optional )
import Text.ParserCombinators.Parsec hiding ( space, newline )
import Harkdown.Tools

data ParagraphContent = Text String
                      | Emphasis String
                      deriving Show

data Harkdown = Paragraph [ParagraphContent]
              | ListItem String
              | HorizontalLineListItem
              | List [Harkdown]
              | HorizontalLine
              | Sequence [Harkdown]
              | CodeBlock String
              | Header String
              | ATXHeader Int String
              deriving Show

newline = char '\n'

space = char ' '

atMost :: Show a => Int -> Parser a -> Parser [a]
atMost 0 p = const [] <$> notFollowedBy p
atMost n p = (:) <$> p <*> (atMost 0 p <|> atMost (n - 1) p)

whitespace = many space

horizontalRuleOf ruleMarker = HorizontalLine <$ try (
  optional space *> optional space *> optional space *> string ruleMarker *>
  whitespace *> string ruleMarker *>
  whitespace *> string ruleMarker *>
  many (whitespace *> string ruleMarker) *> optional newline)

horizontalRule = horizontalRuleOf "*" <|> horizontalRuleOf "_" <|> horizontalRuleOf "-"

listItemBullet = string "* " <|> string "- "

horizontalRuleListItem = HorizontalLineListItem <$ try (listItemBullet *> horizontalRule)

regularListItem = ListItem <$> (listItemBullet *> many (noneOf "\n") <* newline)

listItem = notFollowedBy horizontalRule *> try (horizontalRuleListItem <|> regularListItem)

list = List <$> many1 listItem

paragraphText = Text <$> (many1 (noneOf "\n") <* char '\n')

emphasis = Emphasis <$> try (whitespace *> between (string "*") (string "*") (many1 $ noneOf "*"))

setextHeader = Header <$> try (manyTill anyToken newline <* string "---\n")

paragraphContent = notFollowedBy horizontalRule *> (emphasis <|> paragraphText)

emptyLine = char '\n'

paragraph = Paragraph <$> (many1 paragraphContent <* optional emptyLine)

codeBlock = CodeBlock <$> (try (string "    ") *> many (noneOf "\n") <* string "\n")

atxHeader = ATXHeader <$> try (length <$> atMost 6 (char '#') <* space) <*> manyTill anyToken newline

parser = Sequence <$> many (codeBlock <|> horizontalRule <|> atxHeader <|> setextHeader <|> list <|> paragraph)
