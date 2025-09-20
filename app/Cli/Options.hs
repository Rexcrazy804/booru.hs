module Cli.Options (
  Options (..),
  DownloadOpts (..),
  Commands (..),
  parseOpts,
) where

import Options.Applicative

data Options = Options
  { subcommand :: Commands
  , config :: String
  }

data Commands = Build | Download DownloadOpts

data DownloadOpts = DownloadOpts
  { provider :: String
  , ids :: [String]
  }

parseOpts :: IO Options
parseOpts =
  execParser $
    info
      (optionParser <**> helper)
      (fullDesc <> header "Booru-hs Cli for booru needs")

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

dlOptParser :: Parser DownloadOpts
dlOptParser =
  DownloadOpts
    <$> argument str (metavar "PROVIDER" <> help "the name of the provider to retreive images from")
    <*> some (argument str (metavar "IDS" <> help "list of ids to download"))

commandsParser :: Parser Commands
commandsParser =
  hsubparser
    ( command "build" (info (pure Build) $ progDesc "build the image folder")
        <> command "download" (info (Download <$> dlOptParser) $ progDesc "download images using IDS from a given PROVIDER")
    )
