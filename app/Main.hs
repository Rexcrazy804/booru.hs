module Main where

import Booru.Builtin.Providers (danbooruDonmaiUs, safebooruOrg, specialUrls, zerochanNet)
import Booru.Core.Requests (getProviderMap, requestFile)
import Booru.Schema.Images (
  Identifier (Id),
  Image (Image, resolvedName),
  resolvedName,
 )
import Cli.Commands (Commands (Download), DownloadOpts (..))
import Cli.Options
import qualified Cli.Options as Op
import Control.Monad (forM, forM_)
import qualified Data.ByteString as L
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Options.Applicative

main :: IO ()
main = dispatch =<< execParser opts
 where
  opts =
    info
      (optionParser <**> helper)
      (fullDesc <> header "Booru-hs Cli for booru needs")

dispatch :: Options -> IO ()
dispatch Options{Op.command = Download opts} = getImages opts
dispatch _ = return ()

getImages :: DownloadOpts -> IO ()
getImages DownloadOpts{provider = prv, ids = ids'} = do
  let
    provMap = getProviderMap [danbooruDonmaiUs, specialUrls, safebooruOrg, zerochanNet]
    idnfrs = map Id ids'
  (Just fetchImage) <- return $ M.lookup prv provMap
  imgs' <- forM idnfrs fetchImage
  let imgs = catMaybes imgs'
  forM_ imgs downloadImage

downloadImage :: Image -> IO ()
downloadImage img@Image{resolvedName = name} = do
  rsbod <- requestFile img
  L.writeFile name rsbod
  return ()
