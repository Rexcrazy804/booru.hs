{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Booru.Schema.Filters (
  Filter (..),
  Filters (..),
  Previews (..),
)
where

import Booru.Schema.Images (Tag)
import GHC.Generics (Generic)
import Toml.Schema

data Filter = Filter
  { list :: [Tag]
  , inverted :: Bool
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Filter

-- must mirror what is available to `Category`
data Filters = Filters
  { characters :: Maybe Filter
  , copyrights :: Maybe Filter
  , artists :: Maybe Filter
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Filters

data Previews = Previews
  { characters :: Maybe Filter
  , copyrights :: Maybe Filter
  , artists :: Maybe Filter
  , tags :: Maybe Filter
  , ids :: Maybe Filter
  , ratings :: Maybe Filter
  , providers :: Maybe Filter
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Previews
