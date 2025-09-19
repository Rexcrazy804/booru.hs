module Booru.Preview (
  generatePreview,
  filterImages,
)
where

import Booru.Schema.Filters (Filter (..), Previews (..))
import Booru.Schema.Images (Image (Image), Tag, extractId', file, preview_file, resolvedName)
import qualified Booru.Schema.Images (Image (id))
import qualified Booru.Schema.Images as Img
import Data.Bits (Bits (xor))
import Data.List (intercalate)
import Data.List.Split (chunksOf)

filterImages :: Previews -> [Image] -> [Image]
filterImages pr = filter (applyFilter' pr)

applyFilter' :: Previews -> Image -> Bool
applyFilter' pr img =
  not $
    any
      canFilter
      [ (characters pr, Img.characters img)
      , (copyrights pr, Img.copyrights img)
      , (artists pr, Img.artists img)
      , (tags pr, Img.tags img)
      , (ids pr, extractId' $ Booru.Schema.Images.id img)
      , (ratings pr, [Img.rating img])
      , (providers pr, [Img.provider img])
      ]

canFilter :: (Maybe Filter, [Tag]) -> Bool
canFilter (Nothing, _) = False
canFilter (Just (Filter{list = fts, inverted = inv}), ts) = inv `xor` any (`elem` fts) ts

mdTableHeader :: [String]
mdTableHeader =
  [ "| Column 1 | Column 2 | Column 3 | Column 4 |"
  , "|----------|----------|----------|----------|"
  ]

tabulate :: [String] -> String
tabulate xs = "|" ++ intercalate "|" xs ++ "|"

-- TODO
-- let preview_file be a maybe srting in image spec
toMarkDown :: Image -> String
toMarkDown Image{resolvedName = n, file = f, preview_file = ""} = "![" ++ n ++ "](" ++ f ++ ")"
toMarkDown Image{resolvedName = n, file = f, preview_file = pf} = "[![" ++ n ++ "](" ++ pf ++ ")](" ++ f ++ ")"

generatePreview :: [Image] -> String
generatePreview imgs = unlines $ mdTableHeader ++ tabulatedPreviews
 where
  mdImages = map toMarkDown imgs
  tabulatedPreviews = map tabulate $ chunksOf 4 mdImages
