module Harkdown.HTMLGeneration ( generateHTML ) where

import Harkdown.Parser
import Harkdown.Tools
import Data.List
import Data.List.Utils

joinLines = init . unlines

escapeHTML = replace "<" "&lt;" .
             replace ">" "&gt;" .
             replace "\"" "&quot;"

paragraphHTML (Text text) = escapeHTML text
paragraphHTML (Emphasis text) = "<em>" ++ text ++ "</em>"

inlineHTML (End) = ""
inlineHTML (InlineContent c rest) = paragraphHTML c ++ inlineHTML rest

generateHTML (Paragraph ps) = "<p>" ++ (joinLines . map inlineHTML $ ps) ++ "</p>"
generateHTML (List items) = "<ul>\n" ++ (unlines . map generateHTML $ items) ++ "</ul>"
generateHTML (ListItem content) = "<li>" ++ content ++ "</li>"
generateHTML (HorizontalLineListItem) = "<li><hr/></li>"
generateHTML (HorizontalLine) = "<hr/>"
generateHTML (Sequence items) = flatten . intersperse "\n" . map generateHTML $ items
generateHTML (CodeBlock content) = "<pre><code>" ++ escapeHTML content ++ "\n</code></pre>"
generateHTML (Header n content) = "<h" ++ show n ++ ">" ++ inlineHTML content ++ "</h" ++ show n ++ ">"
generateHTML (Blockquote content) = "<blockquote>\n" ++ generateHTML content ++ "\n</blockquote>"
generateHTML (EmptyHarkdown) = ""
