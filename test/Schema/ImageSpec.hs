{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Schema.ImageSpec (spec) where

import Booru.Schema.Identifier (Identifier (WithNick, id, nickname))
import Booru.Schema.Images
import Data.Text (Text)
import QuoteStr (quoteStr)
import Test.Hspec (Spec, it, shouldBe)
import Toml (Result (..), decode)

fullImage :: Text
fullImage =
  [quoteStr|
  resolvedName = "danbooru-#####"
  provider = "danbooru"
  id = "9969513 kokomi_chibbi"
  file = "https://cdn.donmai.us/original/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"
  preview_file = "https://cdn.donmai.us/180x180/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"
  artists = ["mikurumikurumi"]
  characters = ["sangonomiya_kokomi"]
  copyrights = ["genshin_impact"]
  rating = "g"
  tags = ["1girl", "blush", "bow-shaped_hair", "crying", "crying_with_eyes_open", "gradient_hair", "multicolored_hair", "pink_hair", "purple_eyes", "simple_background", "tears", "trembling"]
  |]

fullImageParsed :: Image
fullImageParsed =
  Image
    { resolvedName = "danbooru-#####"
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

multiImage :: Text
multiImage =
  [quoteStr|
  [[images]]
  resolvedName = "danbooru-#####"
  provider = "danbooru"
  id = "9969513 kokomi_chibbi"
  file = "https://cdn.donmai.us/original/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"
  preview_file = "https://cdn.donmai.us/180x180/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"
  artists = ["mikurumikurumi"]
  characters = ["sangonomiya_kokomi"]
  copyrights = ["genshin_impact"]
  rating = "g"
  tags = ["1girl", "blush", "bow-shaped_hair", "crying", "crying_with_eyes_open", "gradient_hair", "multicolored_hair", "pink_hair", "purple_eyes", "simple_background", "tears", "trembling"]

  [[images]]
  resolvedName = "danbooru-#####"
  provider = "danbooru"
  id = "9969513 kokomi_chibbi"
  file = "https://cdn.donmai.us/original/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"
  preview_file = "https://cdn.donmai.us/180x180/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"
  artists = ["mikurumikurumi"]
  characters = ["sangonomiya_kokomi"]
  copyrights = ["genshin_impact"]
  rating = "g"
  tags = ["1girl", "blush", "bow-shaped_hair", "crying", "crying_with_eyes_open", "gradient_hair", "multicolored_hair", "pink_hair", "purple_eyes", "simple_background", "tears", "trembling"]
  |]

multiImageParsed :: Images
multiImageParsed = Images{images = [fullImageParsed, fullImageParsed]}

spec :: Spec
spec = do
  it "parses simple image" $ decode fullImage `shouldBe` Success [] fullImageParsed
  it "parses multi image" $ decode multiImage `shouldBe` Success [] multiImageParsed
