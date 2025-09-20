module Cli.Dispatcher (dispatch) where

import Cli.Commands.Download
import Cli.Options

dispatch :: Options -> IO ()
dispatch Options{subcommand = Download opts} = getImages opts
dispatch _ = return ()
