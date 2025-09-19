{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Booru.Schema.Images (
  Images (..),
  Image (..),
  Tag,
  Identifier (..),
  extractId,
  extractId',
)
where

import Booru.Schema.Providers (ProviderName)
import Data.Text (unpack)
import GHC.Generics (Generic)
import Toml.Schema

type Tag = String

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
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Image

data Identifier = Id String | WithNick {id :: String, nickname :: String}
  deriving (Eq, Show, Generic)

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

extractId :: Identifier -> String
extractId (Id x) = x
extractId WithNick{id = x} = x

-- returns a list, containing any matchable id information
extractId' :: Identifier -> [Tag]
extractId' (Id x) = [x]
extractId' WithNick{id = x, nickname = n} = [x, n]
