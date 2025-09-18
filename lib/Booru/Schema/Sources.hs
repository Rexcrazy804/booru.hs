{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Booru.Schema.Sources (
  Sources (..),
  Source (..),
  Override (..),
  Filters (..),
  Filter (..),
  Previews (..),
  Identifier (..),
  Tag,
)
where

import Data.Text (unpack)
import GHC.Generics (Generic)
import Toml.Schema

newtype Sources = Sources {sources :: [Source]}
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Sources

type Tag = String

data Source = Source
  { provider :: String
  , ids :: [Identifier]
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

data Identifier = Id String | WithNick {id :: String, nickname :: String}
  deriving (Eq, Show, Ord, Generic)

instance ToValue Identifier where
  toValue (Id id') = toValue id'
  toValue (WithNick id' nick) = toValue $ unwords [id', nick]

instance FromValue Identifier where
  fromValue (Text' l packed)
    | [y, z] <- x = return $ WithNick{id = y, nickname = z}
    | [x'] <- x = return $ Id x'
    | otherwise = failAt l "Expected a string of '<id>' or '<id> <nickname>'"
   where
    x = words $ unpack packed
  fromValue _ = fail "expected a string"
