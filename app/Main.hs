module Main where

import Cli.Dispatcher (dispatch)
import Cli.Options (parseOpts)

main :: IO ()
main = dispatch =<< parseOpts
