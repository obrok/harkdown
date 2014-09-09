module Harkdown.Tools ( split, flatten ) where

flatten :: [String] -> String
flatten = foldl (++) ""

split :: Char -> String -> [String]
split c s = let helper _ [] = []
                helper character string = let x = takeWhile (/= character) string
                                              xs = helper character . drop 1 . dropWhile (/= character) $ string
                                          in x:xs
            in if not (null s) && (head (reverse s) == c)
               then helper c s ++ [[]]
               else helper c s
