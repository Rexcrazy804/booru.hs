{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Booru.Schema.Sources (
  Sources (..),
  Source (..),
  Override (..),
  Filters (..),
  Filter (..),
  Previews (..),
  Tags,
)
where

import Data.Text (Text)
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
  , rating :: Maybe [Tag]
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
  { fcharacters :: Maybe Filter
  , fcopyrights :: Maybe Filter
  , fartists :: Maybe Filter
  , ftags :: Maybe Filter
  , fids :: Maybe Filter
  , fratings :: Maybe Filter
  }
  deriving (Eq, Show, Generic)

toElem :: (ToValue a) => Text -> Maybe a -> [(Text, Value)]
toElem _ Nothing = []
toElem name (Just x) = [name .= x]

instance ToValue Filters where
  toValue = defaultTableToValue

instance ToTable Filters where
  toTable (Filters cha cop art tag id' rat) =
    table $
      concat
        [ toElem "characters" cha
        , toElem "copyrights" cop
        , toElem "artists" art
        , toElem "tags" tag
        , toElem "ids" id'
        , toElem "ratings" rat
        ]

instance FromValue Filters where
  fromValue =
    parseTableFromValue $
      Filters
        <$> optKey "characters"
        <*> optKey "copyrights"
        <*> optKey "artists"
        <*> optKey "tags"
        <*> optKey "ids"
        <*> optKey "ratings"

data Previews = Previews
  { enabled :: Bool
  , pfilters :: Maybe Filters
  }
  deriving (Eq, Show, Generic)

instance ToValue Previews where
  toValue = defaultTableToValue

instance ToTable Previews where
  toTable (Previews enb filts) =
    table (("enabled" .= enb) : toElem "filters" filts)

instance FromValue Previews where
  fromValue =
    parseTableFromValue $
      Previews
        <$> reqKey "enabled"
        <*> optKey "filters"
