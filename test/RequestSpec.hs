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

remoteRequestIdentifier :: Identifier
remoteRequestIdentifier = Id "9590836"

remoteResultImage :: Image
remoteResultImage =
  Image
    { resolvedName = "danbooru-6881072646420435259"
    , provider = "danbooru"
    , id = remoteRequestIdentifier
    , file = "https://cdn.donmai.us/original/24/59/2459e4c38fa4737cb316d76b40e826ed.jpg"
    , preview_file = "https://cdn.donmai.us/180x180/24/59/2459e4c38fa4737cb316d76b40e826ed.jpg"
    , artists = ["ryuuki_(hydrangea)"]
    , characters = ["sangonomiya_kokomi"]
    , copyrights = ["genshin_impact"]
    , rating = "s"
    , tags = ["1girl", "air_bubble", "artist_name", "bare_shoulders", "breasts", "bubble", "cleavage", "gem", "gloves", "light_particles", "light_smile", "long_hair", "looking_at_viewer", "multicolored_eyes", "multicolored_hair", "pink_hair", "ribbon", "sandals", "solo", "thighhighs", "underwater", "white_gloves"]
    }

allowOnline :: IO Bool
allowOnline = do
  skip <- lookupEnv "ENABLE_ONLINE_TESTS"
  return (skip /= Just "1")

spec :: Spec
spec = do
  it "extracts image from static object" $ extractImage danbooruDonmaiUs requestIdentifier booruObject `shouldBe` Just resultImage

  online <- runIO allowOnline
  describe "Online" $ unless online $ do
    it "extracts image from remote object" $ resolveProvider danbooruDonmaiUs remoteRequestIdentifier >>= (`shouldBe` Just remoteResultImage)
