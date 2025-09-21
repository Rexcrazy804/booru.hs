{-# LANGUAGE DuplicateRecordFields #-}

module Cli.Commands.Build (build) where

import Cli.Commands.Download (nullProvider)
import Cli.Common
import Cli.Options (CommonOpts (..))

import Booru.Builtin.Providers (builtinProviders)
import Booru.Core.Overrides (applyOverrides)
import Booru.Core.Parsers
import Booru.Core.Requests (getProviderMap)
import Booru.Schema.Config (Config (..))
import Booru.Schema.Identifier (Identifier)
import Booru.Schema.Images (Image, Images (..))
import qualified Booru.Schema.Images as Img
import Booru.Schema.Providers (ProviderName)
import Booru.Schema.Sources (Source (Source, ids, provider))
import Data.Map (Map, findWithDefault)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

build :: CommonOpts -> IO ()
build CommonOpts{dataDir = d, configDir = cfg} = do
  Config
    { sources = srcs
    , providers = prvs
    , filters = _fls
    , preview_filters = _pfls
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
    (validCImgs, uncachedSrcs) = validateCache srcs cachedImgs
    configPrvs = fromMaybe [] prvs
    provMap = getProviderMap (builtinProviders ++ configPrvs)

  sourceImgMap <- mapM (getMetaData provMap) uncachedSrcs

  let
    overrideImgs = concatMap (uncurry applyOverrides) sourceImgMap
    finalImgs = validCImgs ++ overrideImgs

  writeFile datafile (show $ encode Images{images = finalImgs})
  return ()

-- TODO
-- Modularize this mess

{- | # Yeilds valid cached [Image] and [Source] striped of cache hitting id's
- [Image] -> Images in cached images that are requested in source (eliminates images removed in cfg)
- [Source] -> Source set containing only id's that need to be fetched (i.e. not in cache)
-}
validateCache :: [Source] -> [Image] -> ([Image], [Source])
validateCache srcs imgs = (validImgs, uncachedSrcs)
 where
  imgIdSet = foldr (\img acc -> Set.insert (Img.id img, Img.provider img) acc) Set.empty imgs
  srcIdSet = foldr srcSubfold Set.empty srcs
  srcSubfold src@Source{provider = prv} acc = foldr (\x -> Set.insert (x, prv)) acc $ ids src

  -- its called valid id set but essentially
  -- hold idSets that are in cache, discarding anything that is not mentioned in cache
  validIdSet = imgIdSet `Set.intersection` srcIdSet

  uncachedSrcs = map filterInCache srcs
  filterInCache :: Source -> Source
  filterInCache src@Source{provider = prv} = src{ids = filter (\idnfr -> (idnfr, prv) `Set.notMember` validIdSet) $ ids src}

  -- valid imgs are those images that are in cache and IS requested by the configuration
  validImgs = filter (\x -> (Img.id x, Img.provider x) `Set.member` validIdSet) imgs

getMetaData :: Map ProviderName (Identifier -> IO (Maybe Image)) -> Source -> IO (Source, [Image])
getMetaData pmap src@(Source{ids = idnfrs, provider = prv}) = do
  let metaFetcher = findWithDefault (nullProvider prv) prv pmap
  metaList <- mapM metaFetcher idnfrs

  return (src, catMaybes metaList)
