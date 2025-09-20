module Cli.Common where

import Booru.Core.Parsers (parseFile)
import Booru.Schema.Config
import Data.Maybe (fromMaybe)
import System.Directory (XdgDirectory (XdgConfig, XdgData), getXdgDirectory)
import System.FilePath ((</>))

extractCfg :: Maybe String -> IO Config
extractCfg cfg = do
  booruDir <- getXdgDirectory XdgConfig "booru"
  let
    defaultCfg = booruDir </> "config.toml"
    cfg' = fromMaybe defaultCfg cfg
  parseFile cfg'

getDir :: Maybe String -> IO FilePath
getDir dir = do
  booruDir <- getXdgDirectory XdgData "booru"
  return $ fromMaybe booruDir dir
