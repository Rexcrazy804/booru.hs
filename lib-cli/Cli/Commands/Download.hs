module Cli.Commands.Download (download) where

import Booru.Builtin.Providers (builtinProviders)
import Booru.Core.Requests (getProviderMap, requestFile)
import Booru.Schema.Config (Config (Config, providers))
import Booru.Schema.Identifier (Identifier (Id))
import Booru.Schema.Images (Image (Image, resolvedName), resolvedName)
import Cli.Common
import Cli.Options (CommonOpts (..), DownloadOpts (..))
import Control.Monad (forM, forM_)
import qualified Data.ByteString as L
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)

download :: DownloadOpts -> CommonOpts -> IO ()
download DownloadOpts{provider = prv, ids = ids'} CommonOpts{configDir = cfg} = do
  Config{providers = prvs} <- extractCfg cfg
  let
    configPrv = fromMaybe [] prvs
    provMap = getProviderMap (builtinProviders ++ configPrv)
    idnfrs = map Id ids'
    fetchImage = M.findWithDefault (nullProvider prv) prv provMap

  imgs' <- forM idnfrs fetchImage
  let imgs = catMaybes imgs'
  forM_ imgs downloadImage

downloadImage :: Image -> IO ()
downloadImage img@Image{resolvedName = name} = do
  rsbod <- requestFile img
  L.writeFile name rsbod
  return ()
