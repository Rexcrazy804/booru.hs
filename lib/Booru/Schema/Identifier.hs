{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Booru.Schema.Identifier (
  Identifier (..),
  extractId,
  extractId',
  toResolvedName,
) where

import Booru.Schema.Providers (ProviderName)
import Data.Hashable (hash)
import Data.Text (unpack)
import GHC.Generics (Generic)
import Toml.Schema

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

extractId :: Identifier -> String
extractId (Id x) = x
extractId WithNick{id = x} = x

-- returns a list, containing any matchable id information
extractId' :: Identifier -> [String]
extractId' (Id x) = [x]
extractId' WithNick{id = x, nickname = n} = [x, n]

toResolvedName :: ProviderName -> Identifier -> String
toResolvedName pname id' = pname ++ show (hash $ extractId id')
