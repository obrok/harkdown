module Harkdown.Parser ( InlineItem(..), InlineContent(..), Harkdown(..), parser ) where

import Control.Applicative hiding ( many, (<|>), optional )
import Control.Monad
import Text.ParserCombinators.Parsec hiding ( space, newline )
import Harkdown.Tools
import Data.List

type Language = Maybe String
type Href = String
type Title = Maybe String
type LinkLabel = String

data InlineItem = Text String
                | Emphasis String
                | InlineCode String
                | LinkReference LinkLabel
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
              | CodeBlock Language String
              | Header Int InlineContent
              | Blockquote Harkdown
              | Raw String
              | LinkReferenceDefinition LinkLabel Href Title
              | EmptyHarkdown
              deriving Show

isBlank = all (== ' ')

dropBlanks = dropWhile isBlank . reverse . dropWhile isBlank . reverse

dropIf _ _ [] = []
dropIf 0 _ xs = xs
dropIf n f (x:xs) = if f x
                    then dropIf (n - 1) f xs
                    else x:xs

removeIndent n = foldl (++) "" . intersperse "\n" . map (dropIf n (== ' ')) . split '\n'

newline = char '\n'

space = char ' '

hash = char '#'

minus = char '-'

equals = char '='

backslash = char '\\'

gt = char '>'

lt = char '<'

foreslash = char '/'

tilde = char '~'

backtick = char '`'

lbracket = char '['

rbracket = char ']'

colon = char ':'

doubleQuote = char '"'

atMost1 :: Show a => Int -> Parser a -> Parser [a]
atMost1 0 p = const [] <$> notFollowedBy p
atMost1 n p = (:) <$> p <*> (atMost1 0 p <|> atMost1 (n - 1) p)

atMost :: Show a => Int -> Parser a -> Parser [a]
atMost 0 p = const [] <$> notFollowedBy p
atMost n p = (const [] <$> notFollowedBy p) <|>
             (:) <$> p <*> (atMost 0 p <|> atMost (n - 1) p)

atLeast :: Show a => Int -> Parser a -> Parser [a]
atLeast 0 p = many p
atLeast n p = (:) <$> try p <*> atLeast (n - 1) p

many1Till p end = many1 (notFollowedBy end *> p)

smallIndent = atMost 3 space

whitespace = many space

horizontalRuleOf ruleMarker = HorizontalLine <$ try (
  smallIndent *> string ruleMarker *>
  whitespace *> string ruleMarker *>
  whitespace *> string ruleMarker *>
  many (whitespace *> string ruleMarker *> whitespace) *> newline)

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

inlineCode = InlineCode <$> strip <$> (try (string "```") *> manyTill anyToken (try $ string "```"))

linkReference = LinkReference <$> (lbracket *> many (backslash *> anyToken <|> noneOf "]") <* rbracket)

inlineContentItem = try trailingBackslash <|>
                    try escapedChar <|>
                    try emphasis <|>
                    try inlineCode <|>
                    try linkReference <|>
                    paragraphText

inlineContent = (InlineContent <$>  inlineContentItem <*> (try inlineContent <|> pure End))

headerContentRest = (End <$ try (whitespace *> many hash *> whitespace *> lookAhead newline)) <|>
                    (InlineContent <$> inlineContentItem <*> (try headerContentRest <|> pure End))

headerContent = (InlineContent <$> inlineContentItem <*> (try headerContentRest <|> pure End))

paragraphItem = notFollowedBy horizontalRule *>
                notFollowedBy atxHeader *>
                notFollowedBy fencedBlockOpening *>
                notFollowedBy htmlBlock *>
                whitespace *>
                inlineContent <*
                newline

paragraph = Paragraph <$> (many1 paragraphItem <* optional newline)

codeBlockLine = (try (string "    ") *> many (noneOf "\n") <* newline) <|>
                try (whitespace *> newline *> lookAhead codeBlockLine *> pure "")

codeBlock = CodeBlock Nothing <$> unlines <$> dropBlanks <$> many1 codeBlockLine

fencedBlockOpening = (,,) <$>
  (length <$> try smallIndent) <*>
  (try (atLeast 3 backtick) <|> try (atLeast 3 tilde)) <*>
  try (optional whitespace *> optionMaybe (many1Till (noneOf "~`\n") space) <* manyTill (noneOf "~`\n") newline)

fencedBlockClosing opening = (string opening *> many (char $ head opening) <* notFollowedBy (many1 (noneOf "\n") *> newline)) <|>
                             (eof *> pure "")

fencedCodeBlock = do
  (indent, opening, lang) <- try fencedBlockOpening
  result <- manyTill anyToken (try $ fencedBlockClosing opening)
  optional newline
  return $ CodeBlock lang $ removeIndent indent result

atxHeaderLead = length <$> (smallIndent *> atMost1 6 hash)

emptyAtxHeader = Header <$> try (atxHeaderLead <* whitespace <* many hash <* newline) <*> pure End

atxHeader = Header <$> try (atxHeaderLead <* space) <*> (whitespace *> headerContent <* newline)

setextHeader = (try setextHeader1 <|> try setextHeader2) <* optional newline

setextHeaderBody = smallIndent *> headerContent <* newline <* smallIndent

setextHeader1 = Header 1 <$> setextHeaderBody <* many1 equals <* whitespace <* newline

setextHeader2 = Header 2 <$> setextHeaderBody <* many1 minus <* whitespace <* newline

blockquote = Blockquote <$> (gt *> space *> paragraph)

emptyLine = whitespace *> newline *> pure EmptyHarkdown

htmlBlock = try $ do
  indent <- smallIndent
  lt <- lt
  rest <- manyTill anyToken ((try $ string "\n\n") <|> (eof *> pure ""))
  return $ Raw $ indent ++ (lt:rest)

linkLabel = smallIndent *> lbracket *> many (backslash *> anyToken <|> noneOf "]") <* rbracket <* colon <* whitespace <* optional newline

linkHrefBody = try (lt *> many (backslash *> anyToken <|> noneOf "\n>") <* gt) <|>
               many (noneOf "\n ")

linkHref = whitespace *> linkHrefBody <* whitespace <* optional newline

linkTitle = whitespace *> oneOf "'\"" *> many (noneOf "'\"") <* oneOf "'\"" <* whitespace <* newline

linkReferenceDefinition = LinkReferenceDefinition <$> linkLabel <*> linkHref <*> optionMaybe linkTitle

parser = Sequence <$> many (
  codeBlock <|>
  fencedCodeBlock <|>
  blockquote <|>
  horizontalRule <|>
  emptyAtxHeader <|>
  atxHeader <|>
  setextHeader <|>
  list <|>
  htmlBlock <|>
  try linkReferenceDefinition <|>
  paragraph <|>
  emptyLine)
