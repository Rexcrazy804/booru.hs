{-# LANGUAGE DuplicateRecordFields #-}

module Core.OverrideSpec (spec) where

import Booru.Core.Overrides
import Booru.Schema.Identifier
import Booru.Schema.Images
import qualified Booru.Schema.Images as Img
import Booru.Schema.Sources
import Test.Hspec (Spec, it, shouldBe)

appendOverride :: Override
appendOverride =
  Override
    { identifier = "kokomiIsCute"
    , append = True
    , characters = Just ["kok", "also Kok"]
    , copyrights = Just ["yuanshen"]
    , artists = Just ["notRexcrzy"]
    , tags = Just ["cute", "adorable", "sweet"]
    , rating = Just "explicit"
    }

appendOverrideInvalidId :: Override
appendOverrideInvalidId = appendOverride{identifier = "8668"}

overwriteOverride :: Override
overwriteOverride = appendOverride{append = False}

overWriteOverrideIdMatch :: Override
overWriteOverrideIdMatch = overwriteOverride{identifier = "9969513"}

simpleImg :: Image
simpleImg =
  Image
    { resolvedName = "danbooru|-8916590893211164014"
    , provider = "danbooru"
    , id = WithNick{id = "9969513", nickname = "kokomiIsCute"}
    , file = "https://cdn.donmai.us/original/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"
    , preview_file = "https://cdn.donmai.us/180x180/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"
    , artists = ["mikurumikurumi"]
    , characters = ["sangonomiya_kokomi"]
    , copyrights = ["genshin_impact"]
    , rating = ""
    , tags = ["lovely"]
    }

appendedImage :: Image
appendedImage =
  simpleImg
    { Img.artists = ["mikurumikurumi", "notRexcrzy"]
    , Img.characters = ["sangonomiya_kokomi", "kok", "also Kok"]
    , Img.copyrights = ["genshin_impact", "yuanshen"]
    , Img.rating = "explicit"
    , Img.tags = ["lovely", "cute", "adorable", "sweet"]
    }

overwrittenImage :: Image
overwrittenImage =
  simpleImg
    { Img.artists = ["notRexcrzy"]
    , Img.characters = ["kok", "also Kok"]
    , Img.copyrights = ["yuanshen"]
    , Img.rating = "explicit"
    , Img.tags = ["cute", "adorable", "sweet"]
    }

simpleSource :: Source
simpleSource =
  Source
    { provider = "ignored"
    , ids = [Id "ignored"]
    , overrides = Just [appendOverride]
    }

skippingSource :: Source
skippingSource = simpleSource{overrides = Just [appendOverrideInvalidId]}

idMatchSource :: Source
idMatchSource = simpleSource{overrides = Just [overWriteOverrideIdMatch]}

spec :: Spec
spec = do
  it "appends image with override" $ overrideImage (Just appendOverride) simpleImg `shouldBe` appendedImage
  it "overwrites image with override" $ overrideImage (Just overwriteOverride) simpleImg `shouldBe` overwrittenImage
  it "applies override based on nickname" $ applyOverrides simpleSource [simpleImg] `shouldBe` [appendedImage]
  it "applies override based on id" $ applyOverrides idMatchSource [simpleImg] `shouldBe` [overwrittenImage]
  it "skips images with no associated override" $ applyOverrides skippingSource [simpleImg] `shouldBe` [simpleImg]
