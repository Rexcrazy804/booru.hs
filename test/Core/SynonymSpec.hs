{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.SynonymSpec (spec) where

import Booru.Core.Synonyms (Synonyms', realizeSynonyms, replaceSynonyms, toSynonyms')
import Booru.Schema.Identifier
import Booru.Schema.Images
import Booru.Schema.Synonyms (Synonyms (Synonyms))
import qualified Booru.Schema.Synonyms
import Data.Map (fromList)
import Test.Hspec (Spec, it, shouldBe)

simpleSynonyms :: Synonyms
simpleSynonyms =
  Synonyms
    { characters =
        Just $
          fromList
            [ ("sangonomiya_kokomi", ["kokomi", "sangonomiya kokomi", "sangonomiya_kokomi_(genshin_impact)"])
            , ("yoimiya", ["yoi", "fireworks_girl"])
            ]
    , artists =
        Just $
          fromList
            [ ("elodias", ["little ello"])
            , ("mourncolor", ["color"])
            , ("mikurumikurumi", ["kurukuru"])
            ]
    , copyrights =
        Just $
          fromList
            [ ("genshin_impact", ["genshin impact", "yuanshen"])
            , ("zenless_zone_zero", ["zzz"])
            ]
    , ratings = Just $ fromList [("e", ["explicit", "n$fw"])]
    , tags =
        Just $
          fromList
            [ ("girl", ["1girl", "female"])
            , ("boy", ["1boy", "male"])
            ]
    }

normalSynonyms' :: Synonyms'
normalSynonyms' = toSynonyms' simpleSynonyms

imgOne :: Image
imgOne =
  Image
    { resolvedName = "danbooru|-8916590893211164014"
    , provider = "danbooru"
    , id = WithNick{id = "9969513", nickname = "kokomi_chibbi"}
    , file = "https://cdn.donmai.us/original/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"
    , preview_file = "https://cdn.donmai.us/180x180/bd/6c/bd6cde0ec3b896251118d59b4d0e01b4.jpg"
    , artists = ["kurukuru"]
    , characters = ["sangonomiya_kokomi_(genshin_impact)"]
    , copyrights = ["yuanshen"]
    , rating = "n$fw"
    , tags = ["1girl", "blush"]
    }

imgOne' :: Image
imgOne' =
  imgOne
    { artists = ["mikurumikurumi"]
    , characters = ["sangonomiya_kokomi"]
    , copyrights = ["genshin_impact"]
    , rating = "e"
    , tags = ["girl", "blush"]
    }

imgTwo :: Image
imgTwo =
  imgOne
    { artists = ["little ello", "color"]
    , characters = ["yoi"]
    , copyrights = ["zzz"]
    , rating = "g"
    , tags = tags imgOne ++ ["color"]
    }

imgTwo' :: Image
imgTwo' =
  imgTwo
    { artists = ["elodias", "mourncolor"]
    , characters = ["yoimiya"]
    , copyrights = ["zenless_zone_zero"]
    , rating = "g"
    , tags = tags imgOne' ++ ["color"]
    }

spec :: Spec
spec = do
  it "replaces synonyms in image#1" $ replaceSynonyms normalSynonyms' imgOne `shouldBe` imgOne'
  it "replaces synonyms in image#2" $ replaceSynonyms normalSynonyms' imgTwo `shouldBe` imgTwo'
  it "processes synonyms across multiple images" $ realizeSynonyms simpleSynonyms [imgOne, imgTwo] `shouldBe` [imgOne', imgTwo']
