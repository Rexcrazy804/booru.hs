module Cli.Dispatcher (dispatch) where

import Cli.Commands.Download
import Cli.Common
import Cli.Options
import Control.Monad ((<=<))

-- | reads the options structure and dispatches subcommands
dispatch :: Options -> IO ()
dispatch Options{subcommand = sub, common = copts} = do
  runSubCommand sub copts

runSubCommand :: Commands -> CommonOpts -> IO ()
runSubCommand (Download opts) = getImages opts
runSubCommand _ = (print <=< extractCfg) . configDir
