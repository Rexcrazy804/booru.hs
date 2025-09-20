{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Booru.Schema.Config (
  -- re-exporting Toml parser's Result type
  Result (..),
  Config (..),
  parseConfig,
) where

import Booru.Schema.Filters (Filters)
import Booru.Schema.PFilters (PFilters)
import Booru.Schema.Providers (Provider)
import Booru.Schema.Sources (Source)
import Booru.Schema.Synonyms (Synonyms)

import System.IO (IOMode (ReadMode), hGetContents', withFile)

import Data.Text (pack)
import GHC.Generics (Generic)
import Toml (decode)
import Toml.Schema

data Config = Config
  { sources :: [Source]
  , providers :: Maybe [Provider]
  , filters :: Maybe Filters
  , preview_filters :: Maybe PFilters
  , synonyms :: Maybe Synonyms
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Config

parseConfig :: String -> IO (Result String Config)
parseConfig cfgFile = do
  todoData <- withFile cfgFile ReadMode hGetContents'
  return $ decode (pack todoData)
