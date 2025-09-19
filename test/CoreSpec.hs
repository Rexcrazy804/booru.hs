{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module CoreSpec (spec) where

import Booru.Core (Category (..), genCategory, getImageCat)
import Booru.Schema.Images (Identifier (..), Image (Image))
import qualified Booru.Schema.Images
import qualified Data.Map as M
import qualified Data.Set as S
import Test.Hspec (Spec, it, shouldBe)

simpleImg1 :: Image
simpleImg1 =
  Image
    { resolvedName = "danbooru|-8916590893211164014"
    , provider = "danbooru"
    , id = WithNick{id = "9969513", nickname = "kokomi_chibbi"}
    , file = "https://cdn.donmai.us/original/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"
    , preview_file = "https://cdn.donmai.us/180x180/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"
    , artists = ["mikurumikurumi"]
    , characters = ["sangonomiya_kokomi"]
    , copyrights = ["genshin_impact"]
    , rating = "g"
    , tags = ["1girl", "blush", "bow-shaped_hair", "crying", "crying_with_eyes_open", "gradient_hair", "multicolored_hair", "pink_hair", "purple_eyes", "simple_background", "tears", "trembling"]
    }

simpleImg2 :: Image
simpleImg2 =
  Image
    { resolvedName = "danbooru|-4278074931642418858"
    , provider = "danbooru"
    , id = Id "9868565"
    , file = "https://cdn.donmai.us/original/a7/d2/a7d241b85e78e07da99cec7e7be99bce.jpg"
    , preview_file = "https://cdn.donmai.us/180x180/a7/d2/a7d241b85e78e07da99cec7e7be99bce.jpg"
    , artists = ["togo_uzuki"]
    , characters = ["nilou_(genshin_impact)"]
    , copyrights = ["genshin_impact", "mihoyo", "zenless_zone_zero"]
    , rating = "s"
    , tags = ["1girl", "artist_name", "barefoot", "blue_eyes", "breasts", "brooch", "character_name", "company_connection", "crop_top", "fake_horns", "harem_outfit", "horns", "jewelry", "large_breasts", "limited_palette", "long_hair", "looking_at_viewer", "lying", "midriff", "mindscape_cinema_(zenless_zone_zero)", "navel", "on_back", "open_mouth", "skirt", "smile", "solo", "stomach", "veil", "white_veil"]
    }

simpleImgCat :: Category
simpleImgCat =
  Category
    { artistC = M.fromList [("mikurumikurumi", S.fromList ["danbooru|-8916590893211164014"])]
    , copyrightsC = M.fromList [("genshin_impact", S.fromList ["danbooru|-8916590893211164014"])]
    , charactersC = M.fromList [("sangonomiya_kokomi", S.fromList ["danbooru|-8916590893211164014"])]
    }

combinedImgCat :: Category
combinedImgCat =
  Category
    { artistC =
        M.fromList
          [ ("mikurumikurumi", S.fromList ["danbooru|-8916590893211164014"])
          , ("togo_uzuki", S.fromList ["danbooru|-4278074931642418858"])
          ]
    , copyrightsC =
        M.fromList
          [ ("genshin_impact", S.fromList ["danbooru|-4278074931642418858", "danbooru|-8916590893211164014"])
          , ("mihoyo", S.fromList ["danbooru|-4278074931642418858"])
          , ("zenless_zone_zero", S.fromList ["danbooru|-4278074931642418858"])
          ]
    , charactersC =
        M.fromList
          [ ("nilou_(genshin_impact)", S.fromList ["danbooru|-4278074931642418858"])
          , ("sangonomiya_kokomi", S.fromList ["danbooru|-8916590893211164014"])
          ]
    }

spec :: Spec
spec = do
  it "categorizes simple image" $ getImageCat simpleImg1 `shouldBe` simpleImgCat
  it "generates category from imglist" $ genCategory [simpleImg1, simpleImg2] `shouldBe` combinedImgCat
