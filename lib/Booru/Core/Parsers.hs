module Booru.Core.Parsers (parseFile)
where

import Data.Text (pack)

import System.IO (IOMode (ReadMode), hGetContents', withFile)
import Toml (decode)
import Toml.Schema (Result (..))
import Toml.Schema.FromValue (FromValue)

parseFile :: (FromValue a) => String -> IO a
parseFile file = do
  todoData <- withFile file ReadMode hGetContents'
  processResult $ decode (pack todoData)

processResult :: Result String a -> IO a
processResult (Failure e) = fail $ unwords $ map ("[Error] config: " ++) e
processResult (Success [] c) = return c
processResult (Success wns c) = do
  putStrLn $ unlines $ map ("[Warning] config: " ++) wns
  return c
