module Cli.Utils.Query where

import Booru.Schema.Images (Image (Image, artists, characters, copyrights, tags), Tag)
import Data.Set (union)
import qualified Data.Set as S
import System.Directory (
  createFileLink,
 )
import System.FilePath ((</>))

imgToTags :: Image -> [Tag]
imgToTags Image{characters = cs, copyrights = cps, artists = as, tags = ts} =
  S.toList $
    foldr (union . S.fromList) S.empty [cs, cps, as, ts]

plantImgs :: FilePath -> FilePath -> String -> IO ()
plantImgs ddir pdir rname = createFileLink (ddir </> rname) (pdir </> rname)
