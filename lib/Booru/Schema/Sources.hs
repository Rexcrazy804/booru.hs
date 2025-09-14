{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- ci dispatch test

module Booru.Schema.Sources (
  Sources (..),
  Source (..),
  Override (..),
  Filters (..),
  Filter (..),
  Previews (..),
  Tag,
)
where

import GHC.Generics (Generic)
import Toml.Schema

newtype Sources = Sources {sources :: [Source]}
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Sources

type Tag = String

data Source = Source
  { provider :: String
  , ids :: [String]
  , overrides :: Maybe [Override]
  , filters :: Maybe Filters
  , previews :: Maybe Previews
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Source

data Override = Override
  { identifier :: String
  , append :: Bool
  , characters :: Maybe [Tag]
  , copyrights :: Maybe [Tag]
  , artists :: Maybe [Tag]
  , tags :: Maybe [Tag]
  , rating :: Maybe Tag
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Override

data Filter = Filter
  { list :: [Tag]
  , inverted :: Bool
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Filter

data Filters = Filters
  { characters :: Maybe Filter
  , copyrights :: Maybe Filter
  , artists :: Maybe Filter
  , tags :: Maybe Filter
  , ids :: Maybe Filter
  , ratings :: Maybe Filter
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Filters

data Previews = Previews
  { enabled :: Bool
  , filters :: Maybe Filters
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Previews
