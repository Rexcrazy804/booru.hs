{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Booru.Schema.Images (
  Images (..),
  Image (..),
)
where

import Booru.Schema.Providers (ProviderName)
import Booru.Schema.Sources (Identifier, Tag)
import GHC.Generics (Generic)
import Toml.Schema

newtype Images = Images {images :: [Image]}
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Images

{-
NOTE to self
listend here you forgetfull dummy
the resolved name is gonna be used for quickly correlating
a provider, id pair to a cached provider-hashedID.toml file
-}
data Image = Image
  { resolvedName :: String
  , provider :: ProviderName
  , id :: Identifier
  , file :: String
  , preview_file :: String
  , artists :: [Tag]
  , characters :: [Tag]
  , copyrights :: [Tag]
  , rating :: Tag
  , tags :: [Tag]
  }
  deriving (Eq, Show, Ord, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Image
