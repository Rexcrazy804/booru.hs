module Cli.Dispatcher (dispatch) where

import Booru.Schema.Config (Config, Result (..), parseConfig)
import Cli.Commands.Download
import Cli.Options
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.FilePath ((</>))

-- | reads the options structure and dispatches subcommands
dispatch :: Options -> IO ()
dispatch Options{subcommand = sub, config = cfg} = do
  conf <- extractCfg cfg
  runSubCommand conf sub

runSubCommand :: Config -> Commands -> IO ()
runSubCommand cfg (Download opts) = getImages opts cfg
runSubCommand cfg _ = print cfg

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
