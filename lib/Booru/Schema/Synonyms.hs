{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Booru.Schema.Synonyms (
  Synonyms (..),
  SynoMap,
)
where

import Data.Map
import GHC.Generics (Generic)
import Toml.Schema

data Synonyms = Synonyms
  { artists :: Maybe SynoMap
  , characters :: Maybe SynoMap
  , copyrights :: Maybe SynoMap
  , ratings :: Maybe SynoMap
  , tags :: Maybe SynoMap
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Synonyms

type SynoMap = Map String [String]
