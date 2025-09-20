module Cli.Common where

import Booru.Schema.Config (Config, Result (..), parseConfig)
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.FilePath ((</>))

extractCfg :: String -> IO Config
extractCfg cfg = do
  booruDir <- getUserConfigDir "booru"
  let
    defaultCfg = booruDir </> "config.toml"
    defaultCfg' = "$XDG_CONFIG_HOME/booru/config.toml"
    cfg' = if cfg == defaultCfg' then defaultCfg else cfg
  rs <- parseConfig cfg'
  case rs of
    Failure e -> fail $ unwords $ map ("[Error] config: " ++) e
    Success [] c -> return c
    Success wns c -> do
      putStrLn $ unlines $ map ("[Warning] config: " ++) wns
      return c
