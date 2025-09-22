module Cli.Commands.Query (query) where

import Booru.Core.Category (createTagMap)
import Cli.Options (CommonOpts (..), QueryOpts (..))
import Cli.Utils.Common
import Cli.Utils.Query
import Control.Monad (forM_, when)
import Data.Map (empty, findWithDefault, unionWith)
import Data.Set (intersection, toList, union)
import qualified Data.Set as S
import System.Directory (
  XdgDirectory (XdgCache),
  createDirectoryIfMissing,
  doesDirectoryExist,
  getXdgDirectory,
  removeDirectoryRecursive,
 )

query :: QueryOpts -> CommonOpts -> IO ()
query QueryOpts{tags = ts} CommonOpts{dataDir = d} = do
  (cachedImgs, _, ddir) <- getData d
  tmpPdir <- getXdgDirectory XdgCache "booru-hs"
  dirExists <- doesDirectoryExist tmpPdir
  when dirExists $ removeDirectoryRecursive tmpPdir
  createDirectoryIfMissing True tmpPdir

  let tMaps = map (\img -> createTagMap img $ imgToTags img) cachedImgs
      centralizedMap = foldl (unionWith union) empty tMaps
      tagMatches = map (flip (findWithDefault S.empty) centralizedMap) ts
      queriedImgs = toList $ foldl1 intersection tagMatches

  forM_ queriedImgs (plantImgs ddir tmpPdir)
  putStrLn $ "Images planted at " ++ tmpPdir
