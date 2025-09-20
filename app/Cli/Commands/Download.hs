module Cli.Commands.Download where

import Booru.Builtin.Providers (builtinProviders)
import Booru.Core.Requests (getProviderMap, requestFile)
import Booru.Schema.Images (
  Identifier (Id),
  Image (Image, resolvedName),
  resolvedName,
 )
import Cli.Options (DownloadOpts (..))
import Control.Monad (forM, forM_)
import qualified Data.ByteString as L
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)

getImages :: DownloadOpts -> IO ()
getImages DownloadOpts{provider = prv, ids = ids'} = do
  let
    provMap = getProviderMap builtinProviders
    idnfrs = map Id ids'
    fetchImage = fromMaybe (nullProvider prv) $ M.lookup prv provMap

  imgs' <- forM idnfrs fetchImage
  let imgs = catMaybes imgs'
  forM_ imgs downloadImage

nullProvider :: String -> b -> IO (Maybe a)
nullProvider prv = const $ do
  putStrLn $ "Invalid Provider: " ++ prv
  return Nothing

downloadImage :: Image -> IO ()
downloadImage img@Image{resolvedName = name} = do
  rsbod <- requestFile img
  L.writeFile name rsbod
  return ()
