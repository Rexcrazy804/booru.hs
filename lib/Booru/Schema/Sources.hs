{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Booru.Schema.Sources (
  Sources (..),
  Source (..),
  Override (..),
)
where

import Booru.Schema.Images (Identifier, Tag)
import GHC.Generics (Generic)
import Toml.Schema

newtype Sources = Sources {sources :: [Source]}
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Sources

data Source = Source
  { provider :: String
  , ids :: [Identifier]
  , overrides :: Maybe [Override]
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
