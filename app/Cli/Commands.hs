module Cli.Commands (
  Commands (..),
  DownloadOpts (..),
  commandsParser,
) where

import Options.Applicative

data Commands = Build | Download DownloadOpts

data DownloadOpts = DownloadOpts
  { provider :: String
  , ids :: [String]
  }

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
