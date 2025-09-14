{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Booru.Schema.Providers (
  Providers (..),
  Provider (..),
  ProviderName,
  Attribute (..),
)
where

import Data.List (intercalate)
import Data.Text (unpack)
import GHC.Generics (Generic)
import Helpers (wordsBy)
import Toml.Schema

newtype Providers = Providers {providers :: [Provider]}
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Providers

type ProviderName = String
type JsonAttr = [String]

toJsonAttr :: String -> JsonAttr
toJsonAttr = (`wordsBy` (== '.'))

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

data Attribute = Default String | Attr JsonAttr
  deriving (Eq, Show, Generic)

instance ToValue Attribute where
  toValue (Default xs) = toValue ('$' : xs)
  toValue (Attr xs) = toValue $ intercalate "." xs

instance FromValue Attribute where
  fromValue (Text' l packed)
    | x' : xt <- x, x' == '$' = return (Default xt)
    | x == "" = failAt l "attribute cannot be empty"
    | otherwise = return . Attr $ toJsonAttr x
   where
    x = unpack packed
  fromValue _ = fail "expected simple string or '$' prefixed string"
