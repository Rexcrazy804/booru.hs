module Main where

import Booru.Builtin.Providers (danbooruDonmaiUs)
import Booru.Core.Requests (requestFile, resolveProvider)
import Booru.Schema.Images (
  Identifier (Id),
  Image (Image, resolvedName),
  resolvedName,
 )
import Cli.Options
import Control.Monad (forM, forM_)
import qualified Data.ByteString as L
import Data.Maybe (catMaybes)
import Options.Applicative
import System.Environment (getArgs)

main :: IO ()
main = dispatch =<< execParser opts
 where
  opts =
    info
      (optionParser <**> helper)
      (fullDesc <> header "Booru-hs Cli for booru needs")

dispatch :: Options -> IO ()
dispatch _ = return ()

main' :: IO ()
main' = do
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
