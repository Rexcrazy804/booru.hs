module Cli.Utils.Common where

import Booru.Core.Parsers (parseFile)
import Booru.Schema.Config
import Booru.Schema.Images (Image, Images (Images, images))
import Data.Maybe (fromMaybe)
import System.Directory (
  XdgDirectory (XdgConfig, XdgData),
  createDirectoryIfMissing,
  doesFileExist,
  getHomeDirectory,
  getXdgDirectory,
 )
import System.FilePath ((</>))

-- | Accepts a potential config file path and yeilds the parsed result
extractCfg :: Maybe FilePath -> IO Config
extractCfg cfg = do
  cfg' <- getCfgFile cfg
  parseFile cfg'

-- | Converts a potential config file path to a validated one
getCfgFile :: Maybe FilePath -> IO FilePath
getCfgFile cfg = do
  booruDir <- getXdgDirectory XdgConfig "booru"
  let defaultCfg = booruDir </> "config.toml"
  return $ fromMaybe defaultCfg cfg

{-| accepts a maybe FilePath and yeilds stored data as a tuple.
the resultant tuple contains the following information respectively:
1. list of caches images
2. path to datafile
3. path to image download directory
-}
getData :: Maybe FilePath -> IO ([Image], FilePath, FilePath)
getData dir = do
  booruDir <- getXdgDirectory XdgData "booru"
  let
    ddir = fromMaybe booruDir dir
    datafile = ddir </> "data.toml"
    imgDownloadDir = ddir </> "images"

  createDirectoryIfMissing True imgDownloadDir
  dataExists <- doesFileExist datafile
  Images{images = cachedImgs} <-
    if dataExists
      then parseFile datafile
      else return Images{images = []} :: IO Images

  return (cachedImgs, datafile, imgDownloadDir)

getPlantDir :: Maybe FilePath -> IO FilePath
getPlantDir dir = do
  plantDir <- (</> "booru") . (</> "Pictures") <$> getHomeDirectory
  let actualDir = fromMaybe plantDir dir
  createDirectoryIfMissing True actualDir
  return actualDir

-- | A fetcher that logs that the associated provider is invalid
nullProvider :: String -> a -> IO (Maybe Image)
nullProvider prv = const $ do
  putStrLn $ "Invalid Provider: " ++ prv
  return Nothing
