module Harkdown.HTMLGeneration ( generateHTML ) where

import Harkdown.Parser
import Harkdown.Tools
import Data.List

generateHTML (Paragraph ps) = "<p>" ++ (flatten . intersperse "\n" $ ps) ++ "</p>"
generateHTML (List items) = "<ul>\n" ++ (flatten . map generateHTML $ items) ++ "</ul>"
generateHTML (ListItem content) = "<li>" ++ content ++ "</li>\n"
generateHTML (HorizontalLine) = "<hr/>\n"
generateHTML (Sequence items) = flatten . map generateHTML $ items
generateHTML (CodeBlock content) = "<pre><code>" ++ content ++ "\n</code></pre>"
