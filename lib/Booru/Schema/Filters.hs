{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Booru.Schema.Filters (
  Filter (..),
  Filters (..),
  Previews,
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

data Filters = Filters
  { characters :: Maybe Filter
  , copyrights :: Maybe Filter
  , artists :: Maybe Filter
  , tags :: Maybe Filter
  , ids :: Maybe Filter
  , ratings :: Maybe Filter
  , providers :: Maybe Filter
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Filters

type Previews = Filters
