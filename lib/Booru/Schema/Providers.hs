{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Booru.Schema.Providers (
  Providers (..),
  Provider (..),
  ProviderName,
  Attribute,
)
where

import GHC.Generics (Generic)
import Toml.Schema

newtype Providers = Providers {providers :: [Provider]}
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Providers

type ProviderName = String
type Attribute = String

data Provider = Provider
  { name :: ProviderName
  , url :: String
  , file :: Maybe Attribute
  , preview_file :: Maybe Attribute
  , artists :: Maybe Attribute
  , characters :: Maybe Attribute
  , copyrights :: Maybe Attribute
  , rating :: Maybe Attribute
  , tags :: Maybe Attribute
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Provider
