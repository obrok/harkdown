module Harkdown.HTMLGeneration ( generateHTML ) where

import Harkdown.Parser
import Harkdown.Tools

generateHTML (Paragraph p) = "<p>" ++ p ++ "</p>"
generateHTML (List items) = "<ul>\n" ++ (flatten . map generateHTML $ items) ++ "</ul>"
generateHTML (ListItem content) = "<li>" ++ content ++ "</li>\n"
