import Text.ParserCombinators.Parsec
import Text.Parsec.Prim

flatten :: [String] -> String
flatten = foldl (++) ""

stripEnd :: String -> String
stripEnd = reverse . dropWhile (== ' ') . reverse

tabStop = 4

rjust character cutoff string =
  string ++ take (cutoff - (length string) `mod` cutoff) (repeat character)

charSeq = do
  parts <- (many $ noneOf "\n\t") `sepBy` (oneOf "\t")
  return . stripEnd . flatten . map (rjust ' ' tabStop) $ parts

line = do
  result <- charSeq
  return $ "<p>" ++ result ++ "</p>"

harkdownParser = line

harkdown :: String -> Either ParseError String
harkdown input = parse harkdownParser "could not parse" input

main = do
  input <- getContents
  putStrLn $ case harkdown input of
               Right result -> result
               Left error -> show error

