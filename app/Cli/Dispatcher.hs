module Cli.Dispatcher (dispatch) where

import Cli.Commands.Download
import Cli.Common
import Cli.Options
import Control.Monad ((<=<))

-- | reads the options structure and dispatches subcommands
dispatch :: Options -> IO ()
dispatch Options{subcommand = sub, common = copts} =
  case sub of
    (Download opts) -> download opts
    Build -> (print <=< extractCfg) . configDir
    $ copts
