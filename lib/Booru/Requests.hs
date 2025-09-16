{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Booru.Requests (
  getProviderMap,
  requestJson,
  extractImage,
  resolveProvider,
  toObject,
  toArray,
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
import Data.Vector (toList, (!?))
import Helpers (extractId, wordsBy)
import Network.HTTP.Client.Conduit (Response)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest, setRequestHeader)
import System.Environment (lookupEnv)

getProviderMap :: Providers -> Map ProviderName (Identifier -> IO (Maybe Image))
getProviderMap Providers{providers = prs} = fromList $ foldl mkProviderFns [] prs
 where
  mkProviderFns acc p@(Provider{name = n}) = (n, resolveProvider p) : acc

{- |
# Accepts a provider and an identifier to then return the Image.
- this function is partially applied in the `getProviderMap`
  to yeild a function that takes an identifier and returns the
  `Image`

## Cases
This functional dynamically procs a json request for those
providers that do not come with a `file` attribute.
It then forwards the call down to the `extractImage`

However, in the event there is no such attribute,
the provider and the id is directly passed onto
extractImage with `Nothing` for `Maybe Object`
-}
resolveProvider :: Provider -> Identifier -> IO (Maybe Image)
resolveProvider prvdr@Provider{url = u, file = Just _} id' = do
  let url' = replace (pack "%%ID%%") (pack $ extractId id') (pack u)
  raw <- requestJson (unpack url')
  return $ extractImage prvdr id' raw
resolveProvider prvdr id' = return $ extractImage prvdr id' Nothing

-- | Accepts a url, and potentially returns a json object
requestJson :: String -> IO (Maybe Object)
requestJson url' = do
  request' <- parseRequest url'
  username <- lookupEnv "USER"
  let header = "Booru.hs - " ++ fromMaybe "unkownBooruUser" username
  let request = setRequestHeader "User-Agent" [fromString header] request'
  response <- (httpJSON request :: IO (Response (Maybe Value)))
  return $ getResponseBody response >>= toObject

{- |
# Converts any Value to Maybe Object
If the response contains an object, the object is returned.
however, in the event where a non empty array of objects is returned,
the function returns the first object in the array
-}
toObject :: Value -> Maybe Object
toObject (Atyp.Object x) = Just x
toObject (Atyp.Array x) = x !? 0 >>= toObject
toObject _ = Nothing

toArray :: Value -> Maybe [String]
toArray (Atyp.String x) = Just $ words (unpack x)
toArray (Atyp.Array x)
  | xt <- toList x = Just $ foldl getStr [] xt
 where
  getStr :: [String] -> Value -> [String]
  getStr acc (Atyp.String str) = unpack str : acc
  getStr acc _ = acc
toArray _ = Nothing

{- |
# Constructs the final `Image` representation
Accepts Provider Identifier and a potential json object
to construct information regarding an image into `Image`

## Cases
Two primary casses come into play here,
most providers will facilitate a file attribute
and in turn the provider url is used initially
to fetch information regarding the image.
This information is retreived as a json object,
and the `Provider` strucutre is used to extract information out of it.

On the other hand, for providers like the `urls` provider,
there is no json information to be fetched an inferred,
hence extract image populates default information (provided with $<value>).
and specially processes the provider url substituing %%img%% with id as requried
-}
extractImage :: Provider -> Identifier -> Maybe Object -> Maybe Image
extractImage
  Provider
    { name = nam
    , url = url'
    , preview_file = pf
    , artists = art
    , characters = cha
    , copyrights = cop
    , rating = rat
    , tags = tag
    }
  idnfr
  Nothing =
    Just
      Image
        { resolvedName = nam ++ show (hash url')
        , provider = nam
        , id = idnfr
        , file = unpack $ replace (pack "%%ID%%") (pack $ extractId idnfr) (pack url')
        , preview_file = unwords $ getDefault pf
        , artists = getDefault art
        , characters = getDefault cha
        , copyrights = getDefault cop
        , rating = unwords $ getDefault rat
        , tags = getDefault tag
        }
   where
    getDefault (Just (Default x)) = wordsBy x (== ' ')
    getDefault (Just _) = []
    getDefault Nothing = []
extractImage
  Provider
    { name = nam
    , file = fl
    , preview_file = pf
    , artists = art
    , characters = cha
    , copyrights = cop
    , rating = rat
    , tags = tag
    }
  idnfr
  (Just obj) =
    Just
      Image
        { resolvedName = nam ++ show (hash file')
        , provider = nam
        , id = idnfr
        , file = file'
        , preview_file = unwords $ getAttribute pf
        , artists = getAttribute art
        , characters = getAttribute cha
        , copyrights = getAttribute cop
        , tags = getAttribute tag
        , rating = unwords $ getAttribute rat
        }
   where
    file' = unwords $ getAttribute fl
    getAttribute (Just (Default x)) = wordsBy x (== ' ')
    -- currently we only take the first attribute
    -- TODO figure out a way to extend this
    getAttribute (Just (Attr (x : _))) = fromMaybe [] (getAttribute' x)
    getAttribute _ = []

    getAttribute' :: String -> Maybe [String]
    getAttribute' x = parseMaybe (\ob -> ob .: fromString x) obj >>= toArray
