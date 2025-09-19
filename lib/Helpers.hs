module Helpers where

wordsBy :: String -> (Char -> Bool) -> [String]
wordsBy str delim = aux str
 where
  aux :: String -> [String]
  aux s = case dropWhile delim s of
    "" -> []
    s' -> let (w, s'') = break delim s' in w : aux s''
