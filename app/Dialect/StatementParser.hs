module Dialect.StatementParser
  ( parse
  , strip
  ) where

import Data.String.Utils (strip)

parse :: String -> [String]
parse [] = []
parse haystack =
  parse' haystack ';' []
  where
    parse' :: String -> Char -> [String] -> [String]
    parse' [] _ acc = reverse $ tail $ fmap strip acc
    parse' haystack' needle acc =
      let (statement, rest) = takeStatement haystack' needle ""
      in  parse' rest needle (statement : acc)  

takeStatement :: String -> Char -> String -> (String, String)
takeStatement [] _ acc = (acc, "") 
takeStatement (x:xs) needle acc
  | x == needle = (reverse (x : acc), xs)
  | otherwise = takeStatement xs needle (x : acc)


