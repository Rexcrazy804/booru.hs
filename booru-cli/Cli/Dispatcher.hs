module Cli.Dispatcher (dispatch) where

import Cli.Commands.Build
import Cli.Commands.Download
import Cli.Commands.Generate
import Cli.Commands.Metadata
import Cli.Commands.Preview
import Cli.Commands.Query
import Cli.Options

-- | reads the options structure and dispatches subcommands
dispatch :: Options -> IO ()
dispatch Options{subcommand = sub, common = copts} =
  case sub of
    (Download opts) -> download opts
    (Query opts) -> query opts
    (Metadata opts) -> meta opts
    Build -> build
    Preview -> preview
    GenConf -> generate
    $ copts
