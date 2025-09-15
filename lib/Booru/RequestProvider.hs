{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Booru.RequestProvider (
  getProviderMap,
  requestJson,
) where

import Booru.Schema.Images (Image (..))
import Booru.Schema.Providers (Attribute (..), Provider (..), ProviderName, Providers (..))
import Booru.Schema.Sources (Identifier (..))
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Types as Atyp
import Data.Hashable (hash)
import Data.Map (Map, fromList)
import Data.Maybe
import Data.String (fromString)
import Data.Text (pack, replace, unpack)
import Data.Vector ((!?))
import Helpers (wordsBy)
import Network.HTTP.Client.Conduit (Response)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest, setRequestHeader)

getProviderMap :: Providers -> Map ProviderName (Identifier -> IO (Maybe Image))
getProviderMap Providers{providers = prs} = fromList $ foldl mkProviderFns [] prs
 where
  mkProviderFns acc p@(Provider{name = n}) = (n, resolveProvider p) : acc

resolveProvider :: Provider -> Identifier -> IO (Maybe Image)
resolveProvider prvdr@Provider{url = u, file = Just _} id' = do
  let url' = replace (pack "%%ID%%") (pack $ getId id') (pack u)
  raw <- requestJson (unpack url')
  return $ extractImage prvdr raw
 where
  getId (Id x) = x
  getId WithNick{id = x} = x
resolveProvider prvdr _ = return $ extractImage prvdr Nothing

requestJson :: String -> IO (Maybe Object)
requestJson url' = do
  request' <- parseRequest url'
  let request = setRequestHeader "User-Agent" ["curl/8.14.1"] request'
  response <- (httpJSON request :: IO (Response (Maybe Value)))
  -- handle dumb api's that give you a list of objects
  -- with the first object containing all the data
  -- I am looking at you safeboooru
  return $ case getResponseBody response of
    Just (Atyp.Object x) -> return x
    Just (Atyp.Array x) -> case x !? 0 of
      Just (Atyp.Object x') -> return x'
      _ -> Nothing
    _ -> Nothing

extractImage :: Provider -> Maybe Object -> Maybe Image
extractImage
  Provider
    { name = nam
    , url = url'
    , artists = art
    , characters = cha
    , copyrights = cop
    , rating = rat
    , tags = tag
    }
  Nothing =
    Just
      Image
        { resolvedName = nam ++ show (hash url')
        , id = Id url'
        , artists = getDefault art
        , characters = getDefault cha
        , copyrights = getDefault cop
        , rating = case getDefault rat of
            x : _ -> x
            _ -> []
        , tags = getDefault tag
        }
   where
    getDefault (Just (Default x)) = wordsBy x (== ' ')
    getDefault (Just _) = []
    getDefault Nothing = []
extractImage
  Provider
    { name = nam
    , url = url'
    , artists = art
    , characters = cha
    , copyrights = cop
    , rating = rat
    , tags = tag
    }
  (Just obj) =
    Just
      Image
        { resolvedName = nam ++ show (hash url')
        , id = Id url'
        , artists = getAttribute art
        , characters = getAttribute cha
        , copyrights = getAttribute cop
        , tags = getAttribute tag
        , rating = case getAttribute rat of
            x : _ -> x
            _ -> []
        }
   where
    getAttribute (Just (Default x)) = [x]
    -- currently we only take the first attribute
    -- TODO figure out a way to extend this
    getAttribute (Just (Attr (x : _))) = words $ fromMaybe [] (getAttribute' x)
    getAttribute _ = []

    getAttribute' :: String -> Maybe String
    getAttribute' x = parseMaybe (\ob -> ob .: fromString x) obj
