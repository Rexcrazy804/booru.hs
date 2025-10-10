module Cli.Options (
  Options (..),
  Commands (..),
  CommonOpts (..),
  BuildOpts (..),
  DownloadOpts (..),
  QueryOpts (..),
  MetadataOpts (..),
  MetadataAction (..),
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
  { configFile :: Maybe String
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
              <> help "Directory storing booru raw data"
          )
      )

-- | Available subcommands and their suboption records
data Commands
  = Build BuildOpts
  | Download DownloadOpts
  | Preview
  | Query QueryOpts
  | GenConf
  | Metadata MetadataOpts

-- | scaffolds logic for parsing sub commands
commandsParser :: Parser Commands
commandsParser =
  hsubparser $
    command "build" (info (Build <$> bldOptParser) $ progDesc "build the image folder")
      <> command "gen-config" (info (pure GenConf) $ progDesc "generate example configuration")
      <> command "preview" (info (pure Preview) $ progDesc "generates preview.md into stdout")
      <> command "download" (info (Download <$> dlOptParser) $ progDesc "download images using IDS from a given PROVIDER")
      <> command "metadata" (info (Metadata <$> metaOptParser) $ progDesc "act on stored metadata")
      <> command "query" (info (Query <$> qryOptParser) $ progDesc "retreive images with given TAGS")

data BuildOpts = BuildOpts
  { plantDir :: Maybe String
  , skipCat :: Bool
  }

-- | don't expect me to comment on everything, Baka!
bldOptParser :: Parser BuildOpts
bldOptParser =
  BuildOpts
    <$> optional
      ( strOption
          ( long "plant"
              <> short 'p'
              <> metavar "PLANT_DIR"
              <> help "Directory to plant autocateogrized image folders at. This direcotory is **RECURSIVELY DELETED**"
          )
      )
    <*> switch
      ( long "skip"
          <> short 's'
          <> help "skip the auto categorization step"
      )

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

data MetadataOpts = MetadataOpts
  { action :: MetadataAction
  , resolvedNames :: [String]
  }

metaOptParser :: Parser MetadataOpts
metaOptParser =
  MetadataOpts
    <$> (mUpdateAction <|> mGetAction <|> mRemoveAction)
    <*> some (argument str (metavar "RESOLVEDNAMES" <> help "list of resolvednames"))

data MetadataAction
  = Update
  | Get
  | Remove

mUpdateAction :: Parser MetadataAction
mUpdateAction =
  flag' Update $
    long "update"
      <> short 'u'
      <> help "update stored metadata"

mGetAction :: Parser MetadataAction
mGetAction =
  flag' Get $
    long "get"
      <> short 'g'
      <> help "retreive stored metadata"

mRemoveAction :: Parser MetadataAction
mRemoveAction =
  flag' Remove $
    long "remove"
      <> short 'r'
      <> help "remove stored metadata"
