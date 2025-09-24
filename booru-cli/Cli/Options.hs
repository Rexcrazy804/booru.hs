module Cli.Options (
  Options (..),
  Commands (..),
  CommonOpts (..),
  DownloadOpts (..),
  QueryOpts (..),
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

data Options = Options
  { common :: CommonOpts
  , subcommand :: Commands
  }

optionParser :: Parser Options
optionParser =
  Options
    <$> commonOptsParser
    <*> commandsParser

{- |
# Common Options Record
- **configDir** directory containing the config.toml to be parsed by booru-hs
- **dataDir** directory where booru-hs stores intermediate data
- **plandDir** directory to plant auto categorized result | *this directory is recursively deleted*
-}
data CommonOpts = CommonOpts
  { configDir :: Maybe String
  , dataDir :: Maybe String
  , plantDir :: Maybe String
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
              <> help "Directory storing booru raw data"
          )
      )
    <*> optional
      ( strOption
          ( long "plant"
              <> short 'p'
              <> metavar "PLANT_DIR"
              <> help "Directory to plant autocateogrized image folders at. This direcotory is **RECURSIVELY DELETED**"
          )
      )

-- | Available subcommands and their suboption records
data Commands
  = Build
  | Download DownloadOpts
  | Preview
  | Query QueryOpts
  | GenConf

-- | scaffolds logic for parsing sub commands
commandsParser :: Parser Commands
commandsParser =
  hsubparser $
    command "build" (info (pure Build) $ progDesc "build the image folder")
      <> command "gen-config" (info (pure GenConf) $ progDesc "generate example configuration")
      <> command "preview" (info (pure Preview) $ progDesc "generates preview.md into stdout")
      <> command "download" (info (Download <$> dlOptParser) $ progDesc "download images using IDS from a given PROVIDER")
      <> command "query" (info (Query <$> qryOptParser) $ progDesc "create a folder in the current directory linking images containg given TAGS")

{- | # Download subcommand Options
**provider:** selected provider to request image and metadata from
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

newtype QueryOpts = QueryOpts {tags :: [String]}

qryOptParser :: Parser QueryOpts
qryOptParser =
  QueryOpts
    <$> some (argument str (metavar "TAGS" <> help "list of tags to retreive images for"))
