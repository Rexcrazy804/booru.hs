{-# LANGUAGE DuplicateRecordFields #-}

module Booru.Core.Preview (
  generatePreview,
  filterImages,
)
where

import Booru.Schema.Filters (Filter (..))
import Booru.Schema.Identifier (extractId, extractId')
import Booru.Schema.Images (Image (Image), Tag, file, id, preview_file)
import qualified Booru.Schema.Images as Img
import Booru.Schema.PFilters (PFilters (..))
import Data.Bits (Bits (xor))
import Data.List (intercalate)
import Data.List.Split (chunksOf)

filterImages :: PFilters -> [Image] -> [Image]
filterImages pr = filter (applyFilter' pr)

applyFilter' :: PFilters -> Image -> Bool
applyFilter' pr img =
  not $
    any
      canFilter
      [ (characters pr, Img.characters img)
      , (copyrights pr, Img.copyrights img)
      , (artists pr, Img.artists img)
      , (tags pr, Img.tags img)
      , (ids pr, extractId' $ Img.id img)
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
toMarkDown Image{id = id', file = f, preview_file = ""} = let n = extractId id' in "![" ++ n ++ "](" ++ f ++ ")"
toMarkDown Image{id = id', file = f, preview_file = pf} = let n = extractId id' in "[![" ++ n ++ "](" ++ pf ++ ")](" ++ f ++ ")"

generatePreview :: [Image] -> String
generatePreview imgs = unlines $ mdTableHeader ++ tabulatedPreviews
 where
  mdImages = map toMarkDown imgs
  tabulatedPreviews = map tabulate $ chunksOf 4 mdImages
