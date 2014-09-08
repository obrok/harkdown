module Harkdown.Parser ( InlineItem(..), InlineContent(..), Harkdown(..), parser ) where

import Control.Applicative hiding ( many, (<|>), optional )
import Text.ParserCombinators.Parsec hiding ( space, newline )
import Harkdown.Tools

data InlineItem = Text String
                | Emphasis String
                deriving Show

data InlineContent = InlineContent InlineItem InlineContent
                   | End
                   deriving Show

data Harkdown = Paragraph [InlineContent]
              | ListItem String
              | HorizontalLineListItem
              | List [Harkdown]
              | HorizontalLine
              | Sequence [Harkdown]
              | CodeBlock String
              | Header String
              | ATXHeader Int InlineContent
              deriving Show

newline = char '\n'

space = char ' '

atMost1 :: Show a => Int -> Parser a -> Parser [a]
atMost1 0 p = const [] <$> notFollowedBy p
atMost1 n p = (:) <$> p <*> (atMost1 0 p <|> atMost1 (n - 1) p)

atMost :: Show a => Int -> Parser a -> Parser [a]
atMost 0 p = const [] <$> notFollowedBy p
atMost n p = (const [] <$> notFollowedBy p) <|>
             (:) <$> p <*> (atMost 0 p <|> atMost (n - 1) p)

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

setextHeader = Header <$> try (manyTill anyToken newline <* string "---\n")

escapedChar = Text <$> (:[]) <$> (string "\\" *> anyToken)

paragraphText = Text <$> (:[]) <$> noneOf "\n"

emphasis = Emphasis <$> between (string "*") (string "*") (many1 $ noneOf "*")

inlineContent = (InlineContent <$> (try escapedChar <|> try emphasis <|> paragraphText) <*> (try inlineContent <|> pure End))

paragraph = Paragraph <$> (many1 (notFollowedBy horizontalRule *> whitespace *> inlineContent <* newline) <* optional newline)

codeBlock = CodeBlock <$> (try (string "    ") *> many (noneOf "\n") <* string "\n")

atxHeader = ATXHeader <$> try (length <$> (atMost 3 space *> atMost1 6 (char '#') <* space)) <*> (whitespace *> inlineContent <* newline)

parser = Sequence <$> many (codeBlock <|> horizontalRule <|> atxHeader <|> setextHeader <|> list <|> paragraph)
