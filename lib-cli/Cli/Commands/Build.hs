{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module Cli.Commands.Build (build) where

import Cli.Options (CommonOpts (..))
import Cli.Utils.Build
import Cli.Utils.Common

import Booru.Builtin.Providers (builtinProviders)
import Booru.Core.Category
import Booru.Core.FilterCat (filterCategory)
import Booru.Core.Overrides (applyOverrides)
import Booru.Core.Parsers
import Booru.Core.Requests (getProviderMap)
import Booru.Core.Synonyms (realizeSynonyms)
import Booru.Schema.Config (Config (..))
import Booru.Schema.Images (Images (..))
import Data.Maybe (fromMaybe)

build :: CommonOpts -> IO ()
build CommonOpts{dataDir = d, configDir = cfg, plantDir = p} = do
  Config
    { sources = srcs
    , providers = prvs
    , filters = fls
    , synonyms = syns
    } <-
    extractCfg cfg

  pDir <- getPlantDir p
  (cachedImgs, datafile, imgDownloadDir) <- getData d

  let
    (validCImgs, uncachedSrcs) = validateCache srcs cachedImgs
    configPrvs = fromMaybe [] prvs
    provMap = getProviderMap (builtinProviders ++ configPrvs)

  sourceImgMap <- mapM (\src -> (src,) <$> getMetaData provMap src) uncachedSrcs

  let
    overridenImgs = concatMap (uncurry applyOverrides) sourceImgMap
    synonymAppliedImgs = maybe overridenImgs (`realizeSynonyms` overridenImgs) syns
    finalImgs = validCImgs ++ synonymAppliedImgs
    rawCat = genCategory finalImgs
    category = maybe rawCat (`filterCategory` rawCat) fls

  mapM_ (downloadImage imgDownloadDir) finalImgs
  writeFile datafile (show $ encode Images{images = finalImgs})
  categoryToFs imgDownloadDir pDir category

  return ()
