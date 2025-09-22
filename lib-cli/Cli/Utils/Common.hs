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

extractCfg :: Maybe String -> IO Config
extractCfg cfg = do
  booruDir <- getXdgDirectory XdgConfig "booru"
  let
    defaultCfg = booruDir </> "config.toml"
    cfg' = fromMaybe defaultCfg cfg
  parseFile cfg'

-- alternative that simply returns the correct filepath
getCfgFile :: Maybe String -> IO FilePath
getCfgFile cfg = do
  booruDir <- getXdgDirectory XdgConfig "booru"
  let defaultCfg = booruDir </> "config.toml"
  return $ fromMaybe defaultCfg cfg

getData :: Maybe String -> IO ([Image], FilePath, FilePath)
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

getPlantDir :: Maybe String -> IO FilePath
getPlantDir dir = do
  plantDir <- (</> "booru") . (</> "Pictures") <$> getHomeDirectory
  let actualDir = fromMaybe plantDir dir
  createDirectoryIfMissing True actualDir
  return actualDir

nullProvider :: String -> b -> IO (Maybe a)
nullProvider prv = const $ do
  putStrLn $ "Invalid Provider: " ++ prv
  return Nothing
