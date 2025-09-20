module Cli.Dispatcher (dispatch) where

import Cli.Commands.Download
import Cli.Options

-- | reads the options structure and dispatches subcommands
dispatch :: Options -> IO ()
dispatch Options{subcommand = Download opts} = getImages opts
dispatch _ = return ()
