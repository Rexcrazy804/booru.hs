module Cli.Dispatcher (dispatch) where

import Cli.Commands.Build (build)
import Cli.Commands.Download
import Cli.Options

-- | reads the options structure and dispatches subcommands
dispatch :: Options -> IO ()
dispatch Options{subcommand = sub, common = copts} =
  case sub of
    (Download opts) -> download opts
    Build -> build
    $ copts
