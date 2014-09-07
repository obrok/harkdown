module Harkdown.HTMLGeneration ( generateHTML ) where

import Harkdown.Parser
import Harkdown.Tools
import Data.List

joinLines = init . unlines

paragraphHTML (Text text) = text
paragraphHTML (Emphasis text) = "<em>" ++ text ++ "</em>"

generateHTML (Paragraph ps) = "<p>" ++ (joinLines . map paragraphHTML $ ps) ++ "</p>"
generateHTML (List items) = "<ul>\n" ++ (unlines . map generateHTML $ items) ++ "</ul>"
generateHTML (ListItem content) = "<li>" ++ content ++ "</li>"
generateHTML (HorizontalLineListItem) = "<li><hr/></li>"
generateHTML (HorizontalLine) = "<hr/>"
generateHTML (Sequence items) = flatten . intersperse "\n" . map generateHTML $ items
generateHTML (CodeBlock content) = "<pre><code>" ++ content ++ "\n</code></pre>"
generateHTML (Header text) = "<h2>" ++ text ++ "</h2>\n"
generateHTML (ATXHeader n text) = "<h" ++ show n ++ ">" ++ text ++ "</h" ++ show n ++ ">"
