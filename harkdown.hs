import Text.ParserCombinators.Parsec
import Harkdown.Parser as HP
import Harkdown.HTMLGeneration as HTML
import Harkdown.Tools

stripEnd :: String -> String
stripEnd = reverse . dropWhile (== ' ') . reverse

tabStop = 4

rjust width string = let count = width - length string `mod` width
                     in string ++ (take count $ repeat ' ')

eachLine f = unlines . map f . lines

expandTabs [] = []
expandTabs line = let parts = split '\t' line
                      expanded = map (rjust tabStop) $ init parts
                  in flatten $ expanded ++ [last parts]

preprocess = eachLine expandTabs

harkdown :: String -> Either ParseError HP.Harkdown
harkdown input = parse HP.parser "could not parse" (preprocess input)

main = do
  input <- getContents
  putStrLn $ case harkdown input of
               Right result -> HTML.generateHTML result
               Left error -> show error

