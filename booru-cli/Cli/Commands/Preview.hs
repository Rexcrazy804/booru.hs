module Cli.Commands.Preview (preview) where

import Booru.Core.Preview (filterImages, generatePreview)
import Booru.Schema.Config (Config (..))
import Cli.Options (CommonOpts (..))
import Cli.Utils.Common

preview :: CommonOpts -> IO ()
preview CommonOpts{dataDir = d, configFile = cfg} = do
  Config{preview_filters = pfls} <- extractCfg cfg
  (cachedImgs, _, _) <- getData d
  let filtered = maybe cachedImgs (`filterImages` cachedImgs) pfls
  putStrLn $ generatePreview filtered
