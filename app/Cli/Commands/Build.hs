{-# LANGUAGE DisambiguateRecordFields #-}

module Cli.Commands.Build (build) where

import Cli.Commands.Download (nullProvider)
import Cli.Common
import Cli.Options (CommonOpts (..))

import Booru.Builtin.Providers (builtinProviders)
import Booru.Core.Overrides (applyOverrides)
import Booru.Core.Parsers
import Booru.Core.Requests (getProviderMap)
import Booru.Schema.Config (Config (..))
import Booru.Schema.Images (Identifier, Image (provider), Images (..))
import qualified Booru.Schema.Images (Image (id))
import Booru.Schema.Providers (ProviderName)
import Booru.Schema.Sources (Source (Source, ids, provider))
import Data.Map (Map, findWithDefault)
import Data.Maybe (catMaybes, fromMaybe)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

build :: CommonOpts -> IO ()
build CommonOpts{dataDir = d, configDir = cfg} = do
  Config
    { sources = srcs
    , providers = prvs
    , filters = fls
    , preview_filters = pfls
    } <-
    extractCfg cfg

  cfdir <- getDir d
  createDirectoryIfMissing True (cfdir </> "images")
  let datafile = cfdir </> "data.toml"
  dataExists <- doesFileExist datafile

  Images{images = cachedImgs} <-
    if dataExists
      then parseFile datafile
      else return Images{images = []} :: IO Images

  let
    uncachedSrcs = filterSources srcs cachedImgs
    configPrvs = fromMaybe [] prvs
    provMap = getProviderMap (builtinProviders ++ configPrvs)

  sourceImgMap <- mapM (getMetaData provMap) uncachedSrcs

  let
    overrideImgs = concatMap (uncurry applyOverrides) sourceImgMap
    finalImgs = cachedImgs ++ overrideImgs

  writeFile datafile (show $ encode Images{images = finalImgs})
  return ()

-- | filters cached identifiers in source list
filterSources :: [Source] -> [Image] -> [Source]
filterSources srcs imgs =
  map pruneInCache srcs
 where
  cachedIds = map (\img -> (Booru.Schema.Images.id img, Booru.Schema.Images.provider img)) imgs
  pruneInCache :: Source -> Source
  pruneInCache src@Source{ids = idnfrs, provider = prv} =
    src{ids = filter (\idnfr -> (idnfr, prv) `notElem` cachedIds) idnfrs}

getMetaData :: Map ProviderName (Identifier -> IO (Maybe Image)) -> Source -> IO (Source, [Image])
getMetaData pmap src@(Source{ids = idnfrs, provider = prv}) = do
  let metaFetcher = findWithDefault (nullProvider prv) prv pmap
  metaList <- mapM metaFetcher idnfrs

  return (src, catMaybes metaList)
