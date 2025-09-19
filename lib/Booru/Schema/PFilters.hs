{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Booru.Schema.PFilters (PFilters (..))
where

import Booru.Schema.Filters (Filter)
import GHC.Generics (Generic)
import Toml.Schema

data PFilters = PFilters
  { characters :: Maybe Filter
  , copyrights :: Maybe Filter
  , artists :: Maybe Filter
  , tags :: Maybe Filter
  , ids :: Maybe Filter
  , ratings :: Maybe Filter
  , providers :: Maybe Filter
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable PFilters
