module Cli.Options (
  Options (..),
  optionParser,
) where

import Cli.Commands (Commands, commandsParser)
import Options.Applicative

data Options = Options
  { command :: Commands
  , config :: String
  }

optionParser :: Parser Options
optionParser =
  Options
    <$> commandsParser
    <*> strOption
      ( long "config"
          <> short 'c'
          <> metavar "CONFIG_FILE"
          <> value "/home/rexies/.config/booru/config.toml"
          <> help "Toml file containing booru-hs configuration"
      )
