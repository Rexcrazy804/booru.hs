{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Schema.SynonymSpec (spec) where

import Booru.Schema.Synonyms
import Data.Map (fromList)
import Data.Text (Text)
import QuoteStr (quoteStr)
import Test.Hspec (Spec, it, shouldBe)
import Toml (Result (..), decode)

fullSynonyms :: Text
fullSynonyms =
  [quoteStr|
  [characters]
  sangonomiya_kokomi = ["kokomi", "sangonomiya kokomi", "sangonomiya_kokomi_(genshin_impact)"]
  yoimiya = ["yoi", "fireworks_girl"]

  [tags]
  girl = ["1girl", "female"]

  [copyrights]
  genshin_impact = ["genshin impact", "yuanshen"]

  [artists]
  elodias = ["little ello"]

  [ratings]
  e = ["explicit", "n$fw"]
  |]

fullSynonymsParsed :: Synonyms
fullSynonymsParsed =
  Synonyms
    { characters =
        Just $
          fromList
            [ ("sangonomiya_kokomi", ["kokomi", "sangonomiya kokomi", "sangonomiya_kokomi_(genshin_impact)"])
            , ("yoimiya", ["yoi", "fireworks_girl"])
            ]
    , artists = Just $ fromList [("elodias", ["little ello"])]
    , copyrights = Just $ fromList [("genshin_impact", ["genshin impact", "yuanshen"])]
    , ratings = Just $ fromList [("e", ["explicit", "n$fw"])]
    , tags = Just $ fromList [("girl", ["1girl", "female"])]
    }

spec :: Spec
spec = do
  it "parses exhaustive synonyms" $ decode fullSynonyms `shouldBe` Success [] fullSynonymsParsed
