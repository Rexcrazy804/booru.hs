{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module RequestSpec (spec) where

import Booru.Builtin.Providers
import Booru.Requests (extractImage, resolveProvider, toObject)
import Booru.Schema.Images (Image (..))
import Booru.Schema.Sources (Identifier (..))
import Control.Monad (unless)
import Data.Aeson (Object)
import Data.Aeson.QQ (aesonQQ)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

booruObject :: Maybe Object
booruObject =
  toObject
    [aesonQQ|
    {"id":9969513,"created_at":"2025-09-15T19:27:31.639-04:00","uploader_id":1249148,"score":4,"source":"https://twitter.com/mimizinkomimi/status/1667444598267068417","md5":"bd6cde0ec3b896251118d59b4d0e01b4","last_comment_bumped_at":null,"rating":"g","image_width":2048,"image_height":2048,"tag_string":"1girl blush bow-shaped_hair crying crying_with_eyes_open genshin_impact gradient_hair highres mikurumikurumi multicolored_hair pink_hair purple_eyes sangonomiya_kokomi simple_background tears trembling","fav_count":2,"file_ext":"jpg","last_noted_at":null,"parent_id":null,"has_children":false,"approver_id":1218089,"tag_count_general":12,"tag_count_artist":1,"tag_count_character":1,"tag_count_copyright":1,"file_size":277355,"up_score":4,"down_score":0,"is_pending":false,"is_flagged":false,"is_deleted":false,"tag_count":16,"updated_at":"2025-09-15T22:22:56.926-04:00","is_banned":false,"pixiv_id":null,"last_commented_at":null,"has_active_children":false,"bit_flags":0,"tag_count_meta":1,"has_large":true,"has_visible_children":false,"media_asset":{"id":32474016,"created_at":"2025-09-15T19:21:26.429-04:00","updated_at":"2025-09-15T19:21:27.871-04:00","md5":"bd6cde0ec3b896251118d59b4d0e01b4","file_ext":"jpg","file_size":277355,"image_width":2048,"image_height":2048,"duration":null,"status":"active","file_key":"CwlLIses7","is_public":true,"pixel_hash":"cdfc6df56e32733dea821da7c9bb75ac","variants":[{"type":"180x180","url":"https://cdn.donmai.us/180x180/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg","width":180,"height":180,"file_ext":"jpg"},{"type":"360x360","url":"https://cdn.donmai.us/360x360/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg","width":360,"height":360,"file_ext":"jpg"},{"type":"720x720","url":"https://cdn.donmai.us/720x720/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.webp","width":720,"height":720,"file_ext":"webp"},{"type":"sample","url":"https://cdn.donmai.us/sample/bd/6c/sample-bd6cde0ec3b896251118d59b4d0e01b4.jpg","width":850,"height":850,"file_ext":"jpg"},{"type":"original","url":"https://cdn.donmai.us/original/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg","width":2048,"height":2048,"file_ext":"jpg"}]},"tag_string_general":"1girl blush bow-shaped_hair crying crying_with_eyes_open gradient_hair multicolored_hair pink_hair purple_eyes simple_background tears trembling","tag_string_character":"sangonomiya_kokomi","tag_string_copyright":"genshin_impact","tag_string_artist":"mikurumikurumi","tag_string_meta":"highres","file_url":"https://cdn.donmai.us/original/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg","large_file_url":"https://cdn.donmai.us/sample/bd/6c/sample-bd6cde0ec3b896251118d59b4d0e01b4.jpg","preview_file_url":"https://cdn.donmai.us/180x180/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"}
    |]

booruReqId :: Identifier
booruReqId = WithNick{id = "9969513", nickname = "kokomi_chibbi"}

booruImg :: Image
booruImg =
  Image
    { resolvedName = "danbooru|-8916590893211164014"
    , provider = "danbooru"
    , id = booruReqId
    , file = "https://cdn.donmai.us/original/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"
    , preview_file = "https://cdn.donmai.us/180x180/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"
    , artists = ["mikurumikurumi"]
    , characters = ["sangonomiya_kokomi"]
    , copyrights = ["genshin_impact"]
    , rating = "g"
    , tags = ["1girl", "blush", "bow-shaped_hair", "crying", "crying_with_eyes_open", "gradient_hair", "multicolored_hair", "pink_hair", "purple_eyes", "simple_background", "tears", "trembling"]
    }

safeBooruObject :: Maybe Object
safeBooruObject =
  toObject
    [aesonQQ|
    [{"preview_url":"https:\/\/safebooru.org\/thumbnails\/539\/thumbnail_6fef5929e6e09703b1493c6e5802fa5e4f189639.jpg","sample_url":"https:\/\/safebooru.org\/samples\/539\/sample_6fef5929e6e09703b1493c6e5802fa5e4f189639.jpg","file_url":"https:\/\/safebooru.org\/images\/539\/6fef5929e6e09703b1493c6e5802fa5e4f189639.jpg","directory":539,"hash":"563c6fbe5761ac4ba720ae0da9a15b8a","width":2160,"height":1215,"id":6077620,"image":"6fef5929e6e09703b1493c6e5802fa5e4f189639.jpg","change":1757694622,"owner":"danbooru","parent_id":0,"rating":"general","sample":true,"sample_height":478,"sample_width":850,"score":null,"tags":"6+girls :d animal_ears animal_hairband bird blonde_hair blue_eyes brown_hair cane cat_ears chicken chocolate_chip_cookie closed_eyes clumsy_nun_(diva) cookie diva_(hyxpk) duck english_commentary fake_animal_ears fake_beak food froggy_nun_(diva) grey_hair hedge hedgehog highres holding holding_jar jar little_nuns_(diva) multiple_girls nun old old_woman shy_nun_(diva) smile spicy_nun_(diva) star_nun_(diva) star_ornament traditional_nun triangle_mouth yellow_eyes","source":"https:\/\/www.threads.com\/@diva_hyxpk\/post\/DOghRhxEgeM","status":"active","has_notes":false,"comment_count":0}]
    |]

safeBooruReqId :: Identifier
safeBooruReqId = Id "6077620"

safeBooruImg :: Image
safeBooruImg =
  Image
    { resolvedName = "safebooru|-1258380123441119458"
    , provider = "safebooru"
    , id = safeBooruReqId
    , file = "https://safebooru.org/images/539/6fef5929e6e09703b1493c6e5802fa5e4f189639.jpg"
    , preview_file = "https://safebooru.org/samples/539/sample_6fef5929e6e09703b1493c6e5802fa5e4f189639.jpg"
    , artists = []
    , characters = []
    , copyrights = []
    , rating = "general"
    , tags = ["6+girls", ":d", "animal_ears", "animal_hairband", "bird", "blonde_hair", "blue_eyes", "brown_hair", "cane", "cat_ears", "chicken", "chocolate_chip_cookie", "closed_eyes", "clumsy_nun_(diva)", "cookie", "diva_(hyxpk)", "duck", "english_commentary", "fake_animal_ears", "fake_beak", "food", "froggy_nun_(diva)", "grey_hair", "hedge", "hedgehog", "highres", "holding", "holding_jar", "jar", "little_nuns_(diva)", "multiple_girls", "nun", "old", "old_woman", "shy_nun_(diva)", "smile", "spicy_nun_(diva)", "star_nun_(diva)", "star_ornament", "traditional_nun", "triangle_mouth", "yellow_eyes"]
    }

zeroObject :: Maybe Object
zeroObject =
  toObject
    [aesonQQ|
    { "id": 4585899, "small": "https://s1.zerochan.net/75/49/17/4585899.jpg", "medium": "https://s3.zerochan.net/240/49/17/4585899.jpg", "large": "https://s1.zerochan.net/Honkai.Star.Rail.600.4585899.jpg", "full": "https://static.zerochan.net/Honkai.Star.Rail.full.4585899.jpg", "width": 1092, "height": 1336, "size": 986112, "hash": "72221567cd1eb33ff2600ebdd5dd2c27", "source": "https://www.pixiv.net/en/artworks/135136150", "primary": "Honkai Star Rail", "tags": [ "Female", "Fanart", "Long Hair", "Red Eyes", "Pink Hair", "Blush", "Two Girls", "Yuri", "White Background", "Pixiv", "Duo", "Smile", "Sidelocks", "Simple Background", "Dark Persona", "Looking At Another", "Fanart from Pixiv", "Multi-colored Eyes", "Light Background", "Honkai Star Rail", "March 7th", "oiro ik", "Jellyfish Hair", "Very Long Hair", "Evernight" ] }
    |]

zeroReqId :: Identifier
zeroReqId = Id "4585899"

zeroImg :: Image
zeroImg =
  Image
    { resolvedName = "zerochan|4912039131850589819"
    , provider = "zerochan"
    , id = zeroReqId
    , file = "https://static.zerochan.net/Honkai.Star.Rail.full.4585899.jpg"
    , preview_file = "https://s1.zerochan.net/75/49/17/4585899.jpg"
    , artists = []
    , characters = []
    , copyrights = []
    , rating = ""
    , tags = ["Evernight", "Very Long Hair", "Jellyfish Hair", "oiro ik", "March 7th", "Honkai Star Rail", "Light Background", "Multi-colored Eyes", "Fanart from Pixiv", "Looking At Another", "Dark Persona", "Simple Background", "Sidelocks", "Smile", "Duo", "Pixiv", "White Background", "Yuri", "Two Girls", "Blush", "Pink Hair", "Red Eyes", "Long Hair", "Fanart", "Female"]
    }

allowOnline :: IO Bool
allowOnline = do
  skip <- lookupEnv "ENABLE_ONLINE_TESTS"
  return (skip /= Just "1")

spec :: Spec
spec = do
  it "extracts image from danbooru object" $ extractImage danbooruDonmaiUs booruReqId booruObject `shouldBe` Just booruImg
  it "extracts image from safebooru object" $ extractImage safebooruOrg safeBooruReqId safeBooruObject `shouldBe` Just safeBooruImg
  it "extracts image from zerochan object" $ extractImage zerochanNet zeroReqId zeroObject `shouldBe` Just zeroImg

  online <- runIO allowOnline
  describe "Online" $ unless online $ do
    it "extracts image from danbooru api" $ resolveProvider danbooruDonmaiUs booruReqId >>= (`shouldBe` Just booruImg)
    it "extracts image from safebooru api" $ resolveProvider safebooruOrg safeBooruReqId >>= (`shouldBe` Just safeBooruImg)
    it "extracts image from zerochan api" $ resolveProvider zerochanNet zeroReqId >>= (`shouldBe` Just zeroImg)
