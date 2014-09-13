module Harkdown.HTMLGeneration ( generateHTML ) where

import Harkdown.Parser
import Harkdown.Tools
import Data.List
import qualified Data.Map as M
import Data.List.Utils
import Control.Monad.Trans.State.Lazy

joinLines = init . unlines

escapeHTML = replace "<" "&lt;" .
             replace ">" "&gt;" .
             replace "\"" "&quot;"

paragraphHTML :: InlineItem -> State (M.Map String Harkdown) String

paragraphHTML (Text text) = return $ escapeHTML text
paragraphHTML (Emphasis text) = return $ "<em>" ++ text ++ "</em>"
paragraphHTML (InlineCode code) = return $ "<code>" ++ code ++ "</code>"

paragraphHTML (LinkReference label) = do
  definition <- gets $ M.lookup label
  case definition of
    Just (LinkReferenceDefinition _ href (Just title)) -> return $ "<a href=\"" ++  href ++ "\" title=\"" ++ title ++ "\">" ++ label ++ "</a>"
    Just (LinkReferenceDefinition _ href Nothing) -> return $ "<a href=\"" ++  href ++ "\">" ++ label ++ "</a>"
    Nothing -> return $ "[" ++ label ++ "]"

inlineHTML :: InlineContent -> State (M.Map String Harkdown) String

inlineHTML (End) = return ""
inlineHTML (InlineContent c rest) = do
  restHTML <- inlineHTML rest
  cHTML <- paragraphHTML c
  return $ cHTML ++ restHTML

generateHTMLWithLabels :: Harkdown -> State (M.Map String Harkdown) String

generateHTMLWithLabels (List items) = return $ "<ul>\n" ++ (unlines . map generateHTML $ items) ++ "</ul>"
generateHTMLWithLabels (ListItem content) = return $ "<li>" ++ content ++ "</li>"
generateHTMLWithLabels (HorizontalLineListItem) = return $ "<li><hr/></li>"
generateHTMLWithLabels (HorizontalLine) = return $ "<hr/>"
generateHTMLWithLabels (CodeBlock Nothing content) = return $ "<pre><code>" ++ escapeHTML content ++ "</code></pre>"
generateHTMLWithLabels (CodeBlock (Just lang) content) = return $ "<pre><code class=\"language-" ++ lang ++ "\">" ++ escapeHTML content ++ "</code></pre>"
generateHTMLWithLabels (Blockquote content) = return $ "<blockquote>\n" ++ generateHTML content ++ "\n</blockquote>"
generateHTMLWithLabels (Raw text) = return $ text
generateHTMLWithLabels (EmptyHarkdown) = return $ ""

generateHTMLWithLabels (Sequence items) = do
  content <- mapM generateHTMLWithLabels items
  return $ flatten . intersperse "\n" $ content

generateHTMLWithLabels (Header n c) = do
  content <- inlineHTML c
  return $ "<h" ++ show n ++ ">" ++ content ++ "</h" ++ show n ++ ">"

generateHTMLWithLabels (Paragraph ps) = do
  content <- mapM inlineHTML ps
  return $ "<p>" ++ (joinLines content) ++ "</p>"

generateHTMLWithLabels def@(LinkReferenceDefinition name _ _) = do
  return $ ""

findLabelDefinitions (Sequence items) = foldl M.union M.empty $ map findLabelDefinitions items
findLabelDefinitions def@(LinkReferenceDefinition name _ _) = M.singleton name def
findLabelDefinitions _ = M.empty

generateHTML harkdown = evalState (generateHTMLWithLabels harkdown) (findLabelDefinitions harkdown)
