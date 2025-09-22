module Cli.Utils.Build where

import Booru.Core.Category (Category (..), TagMap)
import Booru.Core.Requests (requestFile)
import Booru.Schema.Identifier (Identifier, extractId, toResolvedName)
import Booru.Schema.Images (Image (Image), resolvedName)
import qualified Booru.Schema.Images as Img
import Booru.Schema.Providers (ProviderName)
import Booru.Schema.Sources (Source (Source, ids, provider))
import Cli.Utils.Common (nullProvider)
import Control.Monad (when)
import qualified Data.ByteString as L
import Data.Foldable (forM_)
import Data.Map (Map, findWithDefault, toList)
import Data.Maybe (catMaybes, isNothing)
import qualified Data.Set as Set
import System.Directory (createDirectoryIfMissing, createFileLink, doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))

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

{- |
First file path is a directory containing resolved images
Second file path is the directory to plant (symlink) categorized images
-}
categoryToFs :: FilePath -> FilePath -> Category -> IO ()
categoryToFs idir pdir cat = do
  pdirExist <- doesDirectoryExist pdir
  when pdirExist $ putStrLn ("Deleting: " ++ pdir) >> removeDirectoryRecursive pdir
  attributeToFs (pdir </> "characters") $ characterC cat
  attributeToFs (pdir </> "copyrights") $ copyrightC cat
  attributeToFs (pdir </> "artists") $ artistC cat
 where
  attributeToFs :: FilePath -> TagMap -> IO ()
  attributeToFs root tmap = do
    forM_
      (toList tmap)
      ( \(tag, imgs) -> do
          let tagF = root </> tag
          putStrLn $ "Sylinking: " ++ tagF
          createDirectoryIfMissing True tagF
          forM_ imgs (\img -> createFileLink (idir </> img) (tagF </> img))
      )
