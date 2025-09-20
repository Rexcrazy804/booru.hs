{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Booru.Schema.Config (
  Config (..),
) where

import Booru.Schema.Filters (Filters)
import Booru.Schema.PFilters (PFilters)
import Booru.Schema.Providers (Provider)
import Booru.Schema.Sources (Source)
import Booru.Schema.Synonyms (Synonyms)

import GHC.Generics (Generic)
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
