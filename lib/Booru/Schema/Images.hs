{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Booru.Schema.Images (
  Images (..),
  Image (..),
)
where

import Booru.Schema.Sources (Tag)
import GHC.Generics (Generic)
import Toml.Schema

newtype Images = Images {images :: [Image]}

data Image = Image
  { resolvedName :: String
  , url :: String
  , artists :: [Tag]
  , characters :: [Tag]
  , copyrights :: [Tag]
  , rating :: Tag
  , tags :: [Tag]
  }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Image
