{-# LANGUAGE DuplicateRecordFields #-}

module Booru.Overrides (
  applyOverrides,
  getOverrideMap,
  overrideImage,
  OverrideMap,
) where

import Booru.Schema.Images
import Booru.Schema.Sources
import qualified Data.Map as M

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
overrideImage
  ( Just
      ( Override
          { append = app
          , artists = art
          , characters = cha
          , copyrights = cop
          , tags = tag
          , rating = rat
          }
        )
    )
  ( Image
      { resolvedName = ires
      , provider = ipro
      , id = iid
      , file = ifil
      , preview_file = ipre
      , artists = iart
      , characters = icha
      , copyrights = icop
      , rating = irat
      , tags = itag
      }
    ) =
    Image
      { resolvedName = ires
      , provider = ipro
      , id = iid
      , file = ifil
      , preview_file = ipre
      , artists = merge art iart
      , characters = merge cha icha
      , copyrights = merge cop icop
      , rating = unwords $ merge ((: []) <$> rat) [irat]
      , tags = merge tag itag
      }
   where
    merge :: Maybe [Tag] -> [Tag] -> [Tag]
    merge Nothing tag' = tag'
    -- if append is true, append, otherwise overrwrite
    merge (Just ovrTag) tag'
      | app = tag' ++ ovrTag
      | otherwise = ovrTag
