module Cli.Common where

import Booru.Schema.Config (Config, Result (..), parseConfig)
import Data.Maybe (fromMaybe)
import System.Directory (XdgDirectory (XdgConfig, XdgData), getXdgDirectory)
import System.FilePath ((</>))

extractCfg :: Maybe String -> IO Config
extractCfg cfg = do
  booruDir <- getXdgDirectory XdgConfig "booru"
  let
    defaultCfg = booruDir </> "config.toml"
    cfg' = fromMaybe defaultCfg cfg
  rs <- parseConfig cfg'
  case rs of
    Failure e -> fail $ unwords $ map ("[Error] config: " ++) e
    Success [] c -> return c
    Success wns c -> do
      putStrLn $ unlines $ map ("[Warning] config: " ++) wns
      return c

getDir :: Maybe String -> IO FilePath
getDir dir = do
  booruDir <- getXdgDirectory XdgData "booru"
  return $ fromMaybe booruDir dir
