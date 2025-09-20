{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Schema.ConfigSpec (spec) where

import Booru.Schema.Config
import Booru.Schema.Filters (Filter (..), Filters (..))
import Booru.Schema.Images (Identifier (..))
import Booru.Schema.PFilters (PFilters (..))
import Booru.Schema.Sources
import Booru.Schema.Synonyms (Synonyms (..))

import Data.Map (fromList)
import Data.Text (Text)
import QuoteStr (quoteStr)
import Test.Hspec (Spec, it, shouldBe)
import Toml (Result (..), decode)

emptyConfig :: Config
emptyConfig =
  Config
    { sources =
        [ ( Source
              { provider = "myprovider"
              , ids = [WithNick{id = "12", nickname = "kok"}, Id "13", Id "14"]
              , overrides =
                  Just
                    [ ( Override
                          { identifier = "kok"
                          , append = True
                          , artists = Nothing
                          , characters = Just ["kokomi"]
                          , copyrights = Just ["genshin_impact"]
                          , tags = Just ["jellyfish", "underwater"]
                          , rating = Just "explicit"
                          }
                      )
                    ]
              }
          )
        ]
    , filters =
        Just $
          Filters
            { characters = Just Filter{list = ["abc", "xyz"], inverted = False}
            , copyrights = Just Filter{list = ["arknights"], inverted = False}
            , artists = Just Filter{list = ["mourncolor", "elodias"], inverted = True}
            }
    , preview_filters =
        Just $
          PFilters
            { characters = Nothing
            , copyrights = Nothing
            , artists = Nothing
            , tags = Nothing
            , ids = Just Filter{list = ["11112"], inverted = False}
            , ratings = Just Filter{list = ["g"], inverted = True}
            , providers = Just Filter{list = ["s34"], inverted = False}
            }
    , providers = Nothing
    , synonyms =
        Just $
          Synonyms
            { characters =
                Just $
                  fromList
                    [ ("sangonomiya_kokomi", ["kokomi", "sangonomiya kokomi", "sangonomiya_kokomi_(genshin_impact)"])
                    , ("yoimiya", ["yoi", "fireworks_girl"])
                    ]
            , artists = Nothing
            , copyrights = Just $ fromList [("genshin_impact", ["genshin impact", "yuanshen"])]
            , ratings = Nothing
            , tags = Just $ fromList [("girl", ["1girl", "female"])]
            }
    }

sampleConfigToml :: Text
sampleConfigToml =
  [quoteStr|
  [[sources]]
  provider = "myprovider"
  ids = ["12 kok", "13", "14"]

  [[sources.overrides]]
  identifier = "kok"
  append = true
  characters = ["kokomi"]
  copyrights = ["genshin_impact"]
  tags = ["jellyfish", "underwater"]
  rating = "explicit"

  [filters]
  artists = { list = ["mourncolor", "elodias"], inverted = true }
  characters = { list = ["abc", "xyz"], inverted = false }
  copyrights = { list = ["arknights"], inverted = false }

  [preview_filters]
  ids = { list = ["11112"], inverted = false }
  ratings = { list = ["g"], inverted = true }
  providers = { list = ["s34"], inverted = false}

  [synonyms.characters]
  sangonomiya_kokomi = ["kokomi", "sangonomiya kokomi", "sangonomiya_kokomi_(genshin_impact)"]
  yoimiya = ["yoi", "fireworks_girl"]

  [synonyms.tags]
  girl = ["1girl", "female"]

  [synonyms.copyrights]
  genshin_impact = ["genshin impact", "yuanshen"]
  |]

spec :: Spec
spec = do
  it "parses sample config" $ decode sampleConfigToml `shouldBe` Success [] emptyConfig
