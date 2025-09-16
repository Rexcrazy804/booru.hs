{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module RequestSpec (spec) where

import Booru.Requests (extractImage, toObject)
import Booru.Schema.Images (Image (..))
import Booru.Schema.Providers (Attribute (..), Provider (..))
import Booru.Schema.Sources (Identifier (..))
import Data.Aeson (Object)
import Data.Aeson.QQ (aesonQQ)
import Test.Hspec (Spec, it, shouldBe)

booruObject :: Maybe Object
booruObject =
  toObject
    [aesonQQ|
    {"id":9969513,"created_at":"2025-09-15T19:27:31.639-04:00","uploader_id":1249148,"score":4,"source":"https://twitter.com/mimizinkomimi/status/1667444598267068417","md5":"bd6cde0ec3b896251118d59b4d0e01b4","last_comment_bumped_at":null,"rating":"g","image_width":2048,"image_height":2048,"tag_string":"1girl blush bow-shaped_hair crying crying_with_eyes_open genshin_impact gradient_hair highres mikurumikurumi multicolored_hair pink_hair purple_eyes sangonomiya_kokomi simple_background tears trembling","fav_count":2,"file_ext":"jpg","last_noted_at":null,"parent_id":null,"has_children":false,"approver_id":1218089,"tag_count_general":12,"tag_count_artist":1,"tag_count_character":1,"tag_count_copyright":1,"file_size":277355,"up_score":4,"down_score":0,"is_pending":false,"is_flagged":false,"is_deleted":false,"tag_count":16,"updated_at":"2025-09-15T22:22:56.926-04:00","is_banned":false,"pixiv_id":null,"last_commented_at":null,"has_active_children":false,"bit_flags":0,"tag_count_meta":1,"has_large":true,"has_visible_children":false,"media_asset":{"id":32474016,"created_at":"2025-09-15T19:21:26.429-04:00","updated_at":"2025-09-15T19:21:27.871-04:00","md5":"bd6cde0ec3b896251118d59b4d0e01b4","file_ext":"jpg","file_size":277355,"image_width":2048,"image_height":2048,"duration":null,"status":"active","file_key":"CwlLIses7","is_public":true,"pixel_hash":"cdfc6df56e32733dea821da7c9bb75ac","variants":[{"type":"180x180","url":"https://cdn.donmai.us/180x180/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg","width":180,"height":180,"file_ext":"jpg"},{"type":"360x360","url":"https://cdn.donmai.us/360x360/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg","width":360,"height":360,"file_ext":"jpg"},{"type":"720x720","url":"https://cdn.donmai.us/720x720/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.webp","width":720,"height":720,"file_ext":"webp"},{"type":"sample","url":"https://cdn.donmai.us/sample/bd/6c/sample-bd6cde0ec3b896251118d59b4d0e01b4.jpg","width":850,"height":850,"file_ext":"jpg"},{"type":"original","url":"https://cdn.donmai.us/original/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg","width":2048,"height":2048,"file_ext":"jpg"}]},"tag_string_general":"1girl blush bow-shaped_hair crying crying_with_eyes_open gradient_hair multicolored_hair pink_hair purple_eyes simple_background tears trembling","tag_string_character":"sangonomiya_kokomi","tag_string_copyright":"genshin_impact","tag_string_artist":"mikurumikurumi","tag_string_meta":"highres","file_url":"https://cdn.donmai.us/original/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg","large_file_url":"https://cdn.donmai.us/sample/bd/6c/sample-bd6cde0ec3b896251118d59b4d0e01b4.jpg","preview_file_url":"https://cdn.donmai.us/180x180/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"}
    |]

danbooruProvider :: Provider
danbooruProvider =
  Provider
    { name = "danbooru"
    , url = "https://danbooru.donmai.us/posts/%%ID%%.json"
    , file = Just $ Attr ["file_url"]
    , preview_file = Just $ Attr ["preview_file_url"]
    , artists = Just $ Attr ["tag_string_artist"]
    , characters = Just $ Attr ["tag_string_character"]
    , copyrights = Just $ Attr ["tag_string_copyright"]
    , tags = Just $ Attr ["tag_string_general"]
    , rating = Just $ Attr ["rating"]
    }

requestIdentifier :: Identifier
requestIdentifier = WithNick{id = "9969513", nickname = "kokomi_chibbi"}

resultImage :: Image
resultImage =
  Image
    { resolvedName = "danbooru-7822550126567983155"
    , provider = "danbooru"
    , id = requestIdentifier
    , file = "https://cdn.donmai.us/original/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"
    , preview_file = "https://cdn.donmai.us/180x180/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"
    , artists = ["mikurumikurumi"]
    , characters = ["sangonomiya_kokomi"]
    , copyrights = ["genshin_impact"]
    , rating = "g"
    , tags = ["1girl", "blush", "bow-shaped_hair", "crying", "crying_with_eyes_open", "gradient_hair", "multicolored_hair", "pink_hair", "purple_eyes", "simple_background", "tears", "trembling"]
    }

spec :: Spec
spec = do
  it "extracts Image from static object" $ extractImage danbooruProvider requestIdentifier booruObject `shouldBe` Just resultImage
