{-# LANGUAGE DuplicateRecordFields #-}

module Cli.Commands.Build (build) where

import Cli.Commands.Download (nullProvider)
import Cli.Common
import Cli.Options (CommonOpts (..))

import Booru.Builtin.Providers (builtinProviders)
import Booru.Core.Overrides (applyOverrides)
import Booru.Core.Parsers
import Booru.Core.Requests (getProviderMap, requestFile)
import Booru.Core.Synonyms (realizeSynonyms)
import Booru.Schema.Config (Config (..))
import Booru.Schema.Identifier (Identifier, extractId, toResolvedName)
import Booru.Schema.Images (Image (Image), Images (..), resolvedName)
import qualified Booru.Schema.Images as Img
import Booru.Schema.Providers (ProviderName)
import Booru.Schema.Sources (Source (Source, ids, provider))
import qualified Data.ByteString as L
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
    , synonyms = syns
    } <-
    extractCfg cfg

  cfdir <- getDir d
  let
    datafile = cfdir </> "data.toml"
    imgDownloadDir = cfdir </> "images"
  createDirectoryIfMissing True imgDownloadDir
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
    overridenImgs = concatMap (uncurry applyOverrides) sourceImgMap
    synonymAppliedImgs = realizeSynonyms syns overridenImgs
    finalImgs = validCImgs ++ synonymAppliedImgs

  mapM_ (downloadImage imgDownloadDir) finalImgs

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
  imgIdSet = foldr (Set.insert . resolvedName) Set.empty imgs
  srcIdSet =
    let srcSubfold src@Source{provider = prv} acc = foldr (Set.insert . toResolvedName prv) acc $ ids src
    in  foldr srcSubfold Set.empty srcs
  validIdSet = imgIdSet `Set.intersection` srcIdSet

  uncachedSrcs =
    let filterInCache src = src{ids = filter ((`Set.notMember` validIdSet) . toResolvedName (provider src)) $ ids src}
    in  map filterInCache srcs

  -- valid imgs are those images that are in cache and IS requested by the configuration
  validImgs = filter ((`Set.member` validIdSet) . Img.resolvedName) imgs

getMetaData :: Map ProviderName (Identifier -> IO (Maybe Image)) -> Source -> IO (Source, [Image])
getMetaData pmap src@(Source{ids = idnfrs, provider = prv}) = do
  let metaFetcher = findWithDefault (nullProvider prv) prv pmap
      logAndFetch id' = do
        putStrLn $ "Retriving metadata for: " ++ extractId id'
        metaFetcher id'
  metaList <- mapM logAndFetch idnfrs

  return (src, catMaybes metaList)

downloadImage :: FilePath -> Image -> IO ()
downloadImage ddir img@Image{resolvedName = name} = do
  let dwnPath = ddir </> name
  imgExists <- doesFileExist dwnPath
  if imgExists
    then return ()
    else do
      rsbod <- requestFile img
      putStrLn $ "Downloading: " ++ name
      L.writeFile dwnPath rsbod
      return ()
