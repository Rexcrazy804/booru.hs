module Cli.Options (
  Options (..),
  DownloadOpts (..),
  CommonOpts (..),
  Commands (..),
  parseOpts,
) where

import Options.Applicative

-- | parses command line arguments given to the program into the Options record
parseOpts :: IO Options
parseOpts =
  execParser $
    info
      (optionParser <**> helper)
      (fullDesc <> header "Booru-hs Cli for booru needs")

{- | # Options Record
**subcommand:** stores the subcommand to run
**config:** contains the path to config.toml
-}
data Options = Options
  { common :: CommonOpts
  , subcommand :: Commands
  }

optionParser :: Parser Options
optionParser =
  Options
    <$> commonOptsParser
    <*> commandsParser

data CommonOpts = CommonOpts
  { configDir :: Maybe String
  , dataDir :: Maybe String
  }

commonOptsParser :: Parser CommonOpts
commonOptsParser =
  CommonOpts
    <$> optional
      ( strOption
          ( long "config"
              <> short 'c'
              <> metavar "CONFIG_FILE"
              <> help "Toml file containing booru-hs configuration"
          )
      )
    <*> optional
      ( strOption
          ( long "data"
              <> short 'd'
              <> metavar "DATA_DIR"
              <> value "$XDG_DATA_HOME/booru"
              <> help "Directory to populate images and other data"
          )
      )

-- | Available subcommands and their option records
data Commands
  = Build
  | Download DownloadOpts

-- | scaffolds logic for parsing sub commands
commandsParser :: Parser Commands
commandsParser =
  hsubparser $
    command "build" (info (pure Build) $ progDesc "build the image folder")
      <> command "download" (info (Download <$> dlOptParser) $ progDesc "download images using IDS from a given PROVIDER")

{- | # Download subcommand Options
**provider:** selected provider to rquest image and metagdata from
**ids:** list of strings containing ids to request
-}
data DownloadOpts = DownloadOpts
  { provider :: String
  , ids :: [String]
  }

-- | don't expect me to comment on everything, Baka!
dlOptParser :: Parser DownloadOpts
dlOptParser =
  DownloadOpts
    <$> argument str (metavar "PROVIDER" <> help "the name of the provider to retreive images from")
    <*> some (argument str (metavar "IDS" <> help "list of ids to download"))
