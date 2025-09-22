module Cli.Dispatcher (dispatch) where

import Cli.Commands.Build
import Cli.Commands.Download
import Cli.Commands.Preview
import Cli.Commands.Query
import Cli.Options

-- | reads the options structure and dispatches subcommands
dispatch :: Options -> IO ()
dispatch Options{subcommand = sub, common = copts} =
  case sub of
    (Download opts) -> download opts
    (Query opts) -> query opts
    Build -> build
    Preview -> preview
    $ copts
