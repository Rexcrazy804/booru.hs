{-# LANGUAGE DuplicateRecordFields #-}

module Booru.Overrides (
  applyOverrides,
  getOverrideMap,
  overrideImage,
  OverrideMap,
) where

import Booru.Schema.Images (Identifier (..), Image (Image, id), Tag)
import qualified Booru.Schema.Images as Img
import Booru.Schema.Sources (Override (..), Source (..))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type OverrideMap = M.Map String Override

applyOverrides :: Source -> [Image] -> [Image]
applyOverrides Source{overrides = Nothing} = Prelude.id
applyOverrides Source{overrides = Just ovrs} = foldl aux []
 where
  ovMap = getOverrideMap ovrs
  aux acc img = overrideImage (getOvrride img ovMap) img : acc

getOvrride :: Image -> OverrideMap -> Maybe Override
getOvrride Image{id = Id x} ovMap = M.lookup x ovMap
getOvrride Image{id = WithNick{id = x, nickname = n}} ovMap
  | ovr@(Just _) <- M.lookup x ovMap = ovr
  | otherwise = M.lookup n ovMap

getOverrideMap :: [Override] -> OverrideMap
getOverrideMap ovrs = M.fromList $ foldl (\acc ovr -> (identifier ovr, ovr) : acc) [] ovrs

overrideImage :: Maybe Override -> Image -> Image
overrideImage Nothing img = img
overrideImage (Just ovrd@Override{append = append'}) img =
  img
    { Img.artists = merge (artists ovrd) (Img.artists img)
    , Img.characters = merge (characters ovrd) (Img.characters img)
    , Img.copyrights = merge (copyrights ovrd) (Img.copyrights img)
    , Img.tags = merge (tags ovrd) (Img.tags img)
    , -- you can't append to rating, its not a list
      Img.rating = fromMaybe (Img.rating img) (rating ovrd)
    }
 where
  merge :: Maybe [Tag] -> [Tag] -> [Tag]
  merge Nothing tag' = tag'
  merge (Just ovrTag) tag'
    | append' = tag' ++ ovrTag
    | otherwise = ovrTag
