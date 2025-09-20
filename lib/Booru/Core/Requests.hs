{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Booru.Core.Requests (
  getProviderMap,
  requestJson,
  requestFile,
  extractImage,
  resolveProvider,
  toObject,
  toArray,
) where

import Booru.Schema.Images (Identifier (..), Image (Image), extractId)
import qualified Booru.Schema.Images as Img
import Booru.Schema.Providers (Attribute (..), Provider (..), ProviderName)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Types as Atyp
import Data.Hashable (hash)
import Data.Map (Map, fromList)
import Data.Maybe
import Data.String (fromString)
import Data.Text (pack, replace, unpack)
import Data.Vector (toList, (!?))
import Network.HTTP.Client.Conduit (Response)
import Network.HTTP.Simple (getResponseBody, httpBS, httpJSON, parseRequest, setRequestHeader)
import System.Environment (lookupEnv)
import Data.ByteString (ByteString)

getProviderMap :: [Provider] -> Map ProviderName (Identifier -> IO (Maybe Image))
getProviderMap prs = fromList $ foldl mkProviderFns [] prs
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

requestFile :: Image -> IO ByteString
requestFile Image{file = f} = do
  request' <- parseRequest f
  username <- lookupEnv "USER"
  let header = "Booru.hs - " ++ fromMaybe "unkownBooruUser" username
  let request = setRequestHeader "User-Agent" [fromString header] request'
  response <- httpBS request
  return $ getResponseBody response

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
extractImage prvdr idnfr Nothing =
  Just
    Image
      { resolvedName = name prvdr ++ '|' : show (hash id')
      , provider = name prvdr
      , id = idnfr
      , file = file'
      , preview_file = unwords $ getDefault $ preview_file prvdr
      , artists = getDefault $ artists prvdr
      , characters = getDefault $ characters prvdr
      , copyrights = getDefault $ copyrights prvdr
      , tags = getDefault $ tags prvdr
      , rating = unwords $ getDefault $ rating prvdr
      }
 where
  id' = extractId idnfr
  file' = unpack $ replace (pack "%%ID%%") (pack id') (pack $ url prvdr)
  getDefault (Just (Default x)) = words x
  getDefault (Just _) = []
  getDefault Nothing = []
extractImage prvdr idnfr (Just obj) =
  return
    Image
      { resolvedName = name prvdr ++ '|' : show (hash $ extractId idnfr)
      , provider = name prvdr
      , id = idnfr
      , file = file'
      , preview_file = unwords $ getAttribute $ preview_file prvdr
      , artists = getAttribute $ artists prvdr
      , characters = getAttribute $ characters prvdr
      , copyrights = getAttribute $ copyrights prvdr
      , tags = getAttribute $ tags prvdr
      , rating = unwords $ getAttribute $ rating prvdr
      }
 where
  file' = unwords $ getAttribute $ file prvdr
  getAttribute (Just (Default x)) = words x
  getAttribute (Just (Attr (x : _))) = fromMaybe [] (getAttribute' x)
  getAttribute _ = []

  -- currently we only take the first attribute
  -- TODO figure out a way to extend this
  getAttribute' :: String -> Maybe [String]
  getAttribute' x = parseMaybe (\ob -> ob .: fromString x) obj >>= toArray
