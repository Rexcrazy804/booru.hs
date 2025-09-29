{-# LANGUAGE DuplicateRecordFields #-}

module Cli.Utils.Build where

import Booru.Core.Category (Category (..), TagMap)
import Booru.Core.Requests (requestFile)
import Booru.Schema.Identifier (Identifier, extractId, toResolvedName)
import Booru.Schema.Images (Image (Image, resolvedName), resolvedName)
import qualified Booru.Schema.Images as Img
import Booru.Schema.Providers (Provider (file, name), ProviderName)
import Booru.Schema.Sources (Source (Source, ids, provider))
import qualified Booru.Schema.Sources as Src
import Cli.Utils.Common (nullProvider)
import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as L
import Data.Map (Map, findWithDefault, fromList, toList)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import qualified Data.Set as Set
import Network.URI (parseURI, pathSegments)
import System.Directory (createDirectoryIfMissing, createFileLink, doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))

{- | Accepts sources and image list to return cache validated tuple of the same
- [[Image]] -> Images in cached images that are requested in source (eliminates images removed in cfg)
- [[Source]] -> Source set containing only id's that need to be fetched (i.e. not in cache)
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
    let filterInCache src = src{ids = filter ((`Set.notMember` validIdSet) . toResolvedName (Src.provider src)) $ ids src}
    in  map filterInCache srcs

  validImgs = filter ((`Set.member` validIdSet) . Img.resolvedName) imgs

-- | A simple wrapper funciton that does some logging to stdout
getMetaData :: Map ProviderName (Identifier -> IO (Maybe Image)) -> Source -> IO [Image]
getMetaData pmap Source{ids = idnfrs, provider = prv} = do
  let metaFetcher = findWithDefault (nullProvider prv) prv pmap
      logAndFetch id' = do
        let exid = extractId id'
        putStrLn $ "Retriving metadata for: " ++ exid
        img <- metaFetcher id'
        when (isNothing img) $ putStrLn ("Unable to retreive: " ++ exid)
        return img
  metaList <- mapM logAndFetch idnfrs
  return $ catMaybes metaList

{- | Downloads images into 'FilePath' when it does not contain the image
- the function leveragtes 'Image.resolvedName' to identify existing files
-}
downloadImage :: FilePath -> Image -> IO ()
downloadImage ddir img@Image{resolvedName = name'} = do
  let dwnPath = ddir </> name'
  imgExists <- doesFileExist dwnPath
  unless imgExists $ do
    rsbod <- requestFile img
    putStrLn $ "Downloading: " ++ name'
    L.writeFile dwnPath rsbod

{- |
First file path is a directory containing resolved images
Second file path is the directory to plant (symlink) categorized images
-}
categoryToFs :: FilePath -> FilePath -> [Provider] -> [Image] -> Category -> IO ()
categoryToFs idir pdir prvs imgs cat = do
  pdirExist <- doesDirectoryExist pdir
  when pdirExist $ putStrLn ("Deleting: " ++ pdir) >> removeDirectoryRecursive pdir
  attributeToFs (pdir </> "characters") $ characterC cat
  attributeToFs (pdir </> "copyrights") $ copyrightC cat
  attributeToFs (pdir </> "artists") $ artistC cat
 where
  rnMap = toRnameMap imgs
  sPrvs = filterSpecialProviders prvs

  attributeToFs :: FilePath -> TagMap -> IO ()
  attributeToFs root tmap = forM_ (toList tmap) $ createTagFolder root

  createTagFolder root (tag, rnames) = do
    let tagF = root </> tag
        toSymLink rname =
          createFileLink
            (idir </> rname)
            (tagF </> fromMaybe rname (Map.lookup rname rnMap >>= getFname' sPrvs))
    putStrLn $ "Sylinking: " ++ tagF
    createDirectoryIfMissing True tagF
    forM_ rnames toSymLink

filterSpecialProviders :: [Provider] -> [ProviderName]
filterSpecialProviders prvs = map name $ filter (isNothing . file) prvs

getFname :: Image -> Maybe String
getFname Image{file = fURI'} = last . pathSegments <$> parseURI fURI'

getFname' :: [ProviderName] -> Image -> Maybe String
getFname' prvs Image{file = fURI', provider = prv, id = id'} = do
  fURI <- parseURI fURI'
  let fullName = last (pathSegments fURI)
      (_, ftype) = span (/= '.') fullName
  -- prvs here is a list of special providers
  -- these providers don't have meaning full ids :<
  if prv `elem` prvs
    then return fullName
    else return $ prv ++ extractId id' ++ ftype

toRnameMap :: [Image] -> Map String Image
toRnameMap img = fromList $ foldl (\acc cur -> (resolvedName cur, cur) : acc) [] img
