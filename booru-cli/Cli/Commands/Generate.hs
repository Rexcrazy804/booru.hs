module Cli.Commands.Generate (generate) where

import Cli.Options (CommonOpts (..))
import Cli.Utils.Common (getCfgFile)
import Control.Monad (unless, when)
import Paths_booru_hs (getDataFileName)
import System.Directory (doesFileExist)

generate :: CommonOpts -> IO ()
generate CommonOpts{configDir = cfg} = do
  exampleCfg <- getDataFileName "examples/config.toml"
  contents <- readFile exampleCfg
  cfgPath <- getCfgFile cfg
  hasCfg <- doesFileExist cfgPath
  unless hasCfg $ do
    writeFile cfgPath contents
    putStrLn $ "Wrote example config to " ++ cfgPath
  when hasCfg $ putStrLn $ "Configuration exists at " ++ cfgPath ++ " action skipped!"
