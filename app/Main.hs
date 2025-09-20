module Main where

import Booru.Builtin.Providers (danbooruDonmaiUs)
import Booru.Core.Requests (requestFile, resolveProvider)
import Booru.Schema.Images (
  Identifier (Id),
  Image (Image, resolvedName),
  resolvedName,
 )
import Control.Monad (forM, forM_)
import qualified Data.ByteString as L
import Data.Maybe (catMaybes)
import System.Environment (getArgs)

main :: IO ()
main = do
  ids <- getArgs
  let booruFetcher = resolveProvider danbooruDonmaiUs
      idnfrs = map Id ids
  imgs' <- forM idnfrs booruFetcher
  let imgs = catMaybes imgs'
  forM_ imgs downloadImage

downloadImage :: Image -> IO ()
downloadImage img@Image{resolvedName = name} = do
  rsbod <- requestFile img
  L.writeFile name rsbod
  return ()
