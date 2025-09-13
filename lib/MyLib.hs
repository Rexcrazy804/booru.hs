{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where

import Data.Text
import QuoteStr (quoteStr)
import Toml (parse)

fruitStr :: Text
fruitStr =
  [quoteStr|
  [[fruits]]
  name = "apple"

  [fruits.physical]  # subtable
  color = "red"
  shape = "round"

  [[fruits.varieties]]  # nested array of tables
  name = "red delicious"

  [[fruits.varieties]]
  name = "granny smith"

  [[fruits]]
  name = "banana"

  [[fruits.varieties]]
  name = "plantain"
  |]

someFunc :: IO ()
someFunc = do
  print $ parse fruitStr
