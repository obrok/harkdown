import Text.ParserCombinators.Parsec
import Text.Parsec.Prim

flatten :: [String] -> String
flatten = foldl (++) ""

stripEnd :: String -> String
stripEnd = reverse . dropWhile (== ' ') . reverse

tabStop = 4

rjust width string = let count = width - length string `mod` width
                     in string ++ (take count $ repeat ' ')

eachLine f = unlines . map f . lines

split :: Char -> String -> [String]
split _ [] = []
split character string = let x = takeWhile (/= character) string
                             xs = split character . drop 1 . dropWhile (/= character) $ string
                         in x:xs

expandTabs line = let parts = split '\t' line
                      expanded = map (rjust tabStop) parts
                  in stripEnd . flatten $ expanded

preprocess = eachLine expandTabs

harkdown :: String -> Either ParseError String
harkdown input = Right $ "<p>" ++ (init $ preprocess input) ++ "</p>"

main = do
  input <- getContents
  putStrLn $ case harkdown input of
               Right result -> result
               Left error -> show error

