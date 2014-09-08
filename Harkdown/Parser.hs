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
              | Header Int InlineContent
              | Blockquote Harkdown
              | EmptyHarkdown
              deriving Show

newline = char '\n'

space = char ' '

hash = char '#'

minus = char '-'

equals = char '='

backslash = char '\\'

gt = char '>'

atMost1 :: Show a => Int -> Parser a -> Parser [a]
atMost1 0 p = const [] <$> notFollowedBy p
atMost1 n p = (:) <$> p <*> (atMost1 0 p <|> atMost1 (n - 1) p)

atMost :: Show a => Int -> Parser a -> Parser [a]
atMost 0 p = const [] <$> notFollowedBy p
atMost n p = (const [] <$> notFollowedBy p) <|>
             (:) <$> p <*> (atMost 0 p <|> atMost (n - 1) p)

smallIndent = atMost 3 space

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

escapedChar = Text <$> (:[]) <$> (backslash *> anyToken)

paragraphText = Text <$> (:[]) <$> noneOf "\n"

trailingBackslash = Text <$> (backslash *> lookAhead newline *> pure "\\")

emphasis = Emphasis <$> between (string "*") (string "*") (many1 $ noneOf "*")

inlineContentItem = try trailingBackslash <|>
                    try escapedChar <|>
                    try emphasis <|>
                    paragraphText

inlineContent = (InlineContent <$>  inlineContentItem <*> (try inlineContent <|> pure End))

headerContent = (End <$ try (whitespace *> many1 hash *> whitespace *> lookAhead newline)) <|>
                (InlineContent <$> inlineContentItem <*> (try headerContent <|> pure End))

paragraphItem = notFollowedBy horizontalRule *>
                notFollowedBy atxHeader *>
                whitespace *>
                inlineContent <*
                newline

paragraph = Paragraph <$> (many1 paragraphItem <* optional newline)

codeBlockLine = (try (string "    ") *> many (noneOf "\n") <* newline) <|>
                try (whitespace *> newline *> lookAhead codeBlockLine *> pure "")

codeBlock = CodeBlock <$> init <$> unlines <$> many1 codeBlockLine

atxHeaderLead = length <$> (smallIndent *> atMost1 6 hash)

emptyAtxHeader = Header <$> try (atxHeaderLead <* whitespace <* newline) <*> pure End

atxHeader = Header <$> try (atxHeaderLead <* space) <*> (whitespace *> headerContent <* newline)

setextHeader = (try setextHeader1 <|> try setextHeader2) <* optional newline

setextHeaderBody = smallIndent *> headerContent <* newline <* smallIndent

setextHeader1 = Header 1 <$> setextHeaderBody <* many1 equals <* newline

setextHeader2 = Header 2 <$> setextHeaderBody <* many1 minus <* newline

blockquote = Blockquote <$> (gt *> space *> paragraph)

emptyLine = whitespace *> newline *> pure EmptyHarkdown

parser = Sequence <$> many (codeBlock <|> blockquote <|> horizontalRule <|> emptyAtxHeader <|> atxHeader <|> setextHeader <|> list <|> paragraph <|> emptyLine)
