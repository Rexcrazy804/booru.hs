module Cli.Commands.Preview (preview) where

import Booru.Core.Preview (filterImages, generatePreview)
import Booru.Schema.Config (Config (..))
import Cli.Common
import Cli.Options (CommonOpts (..))

preview :: CommonOpts -> IO ()
preview CommonOpts{dataDir = d, configDir = cfg} = do
  Config{preview_filters = pfls} <- extractCfg cfg
  (cachedImgs, _, _) <- getData d
  let filtered = filterImages pfls cachedImgs
  putStrLn $ generatePreview filtered

