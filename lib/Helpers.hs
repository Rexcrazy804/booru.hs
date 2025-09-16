{-# LANGUAGE DisambiguateRecordFields #-}

module Helpers where

import Booru.Schema.Sources (Identifier (..))

wordsBy :: String -> (Char -> Bool) -> [String]
wordsBy str delim = aux str
 where
  aux :: String -> [String]
  aux s = case dropWhile delim s of
    "" -> []
    s' -> let (w, s'') = break delim s' in w : aux s''

extractId :: Identifier -> String
extractId (Id x) = x
extractId WithNick{id = x} = x
