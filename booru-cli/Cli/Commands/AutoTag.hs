{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Cli.Commands.AutoTag (autotag) where

import Cli.Options (AutoTagOpts (..), CommonOpts (..))
import Cli.Utils.Build (downloadImage)
import Cli.Utils.Common (extractCfg, getData, nullProvider)

import Booru.Builtin.Providers (builtinProviders)
import Booru.Core.Requests (getProviderMap, requestTags)
import Booru.Schema.Config (Config (Config, providers))
import Booru.Schema.Identifier (Identifier (Id))
import Booru.Schema.Images (Image (resolvedName))
import Control.Monad (forM_, unless, when)
import Data.Map (findWithDefault, toList)
import Data.Maybe (fromMaybe)
import GHC.Float (float2Double)
import System.FilePath ((</>))

autotag :: AutoTagOpts -> CommonOpts -> IO ()
autotag AutoTagOpts{provider = prv, id = imgId, threshold = thres, showT = sht, apiUrl = url} CommonOpts{dataDir = d, configFile = cfg} = do
  Config{providers = prvs} <- extractCfg cfg
  (_, _, imgDownloadDir) <- getData d

  let configPrvs = fromMaybe [] prvs
      provMap = getProviderMap (builtinProviders ++ configPrvs)
      fetchImage = findWithDefault (nullProvider prv) prv provMap

  Just img <- fetchImage (Id imgId)
  let imgFile = imgDownloadDir </> img.resolvedName

  mapM_ (downloadImage imgDownloadDir) [img]
  putStrLn $ "Requesting tags for: " ++ imgId
  tagMap <- requestTags imgFile url

  when (null tagMap) $ putStrLn "welp"

  forM_
    (toList tagMap)
    ( \(tag, ts) -> unless (ts < float2Double thres) $ do
        when sht $ putStr $ take 4 (show ts) ++ " "
        putStrLn $ "\"" ++ tag ++ "\","
    )
