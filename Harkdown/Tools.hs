module Harkdown.Tools ( flatten ) where

flatten :: [String] -> String
flatten = foldl (++) ""
