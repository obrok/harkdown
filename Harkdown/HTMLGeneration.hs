module Harkdown.HTMLGeneration ( generateHTML ) where

import Harkdown.Parser
import Harkdown.Tools
import Data.List

paragraphHTML (Text text) = text
paragraphHTML (Emphasis text) = "<em>" ++ text ++ "</em>"

generateHTML (Paragraph ps) = "<p>" ++ (flatten . intersperse "\n" . map paragraphHTML $ ps) ++ "</p>"
generateHTML (List items) = "<ul>\n" ++ (flatten . map generateHTML $ items) ++ "</ul>"
generateHTML (ListItem content) = "<li>" ++ content ++ "</li>\n"
generateHTML (HorizontalLine) = "<hr/>"
generateHTML (Sequence items) = flatten . intersperse "\n" . map generateHTML $ items
generateHTML (CodeBlock content) = "<pre><code>" ++ content ++ "\n</code></pre>"
