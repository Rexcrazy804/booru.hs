{-# LANGUAGE OverloadedStrings #-}

module Schema.SourceSpec (spec) where

import Booru.Schema.Sources
import Data.Text (Text)
import QuoteStr (quoteStr)
import Test.Hspec (Spec, it, shouldBe)
import Toml (Result (..), decode)

fullSource :: Text
fullSource =
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
  ratings = ["explicit"]

  [sources.filters]
  characters = { list = ["abc", "xyz"], inverted = false }
  copyrights = { list = ["arknights"], inverted = false }
  artists = { list = ["mourncolor", "elodias"], inverted = true }
  tags = { list = ["bird", "horse"], inverted = false }
  ids = { list = ["11112"], inverted = false }
  ratings = { list = ["g"], inverted = true }

  [sources.previews]
  enabled = true
  characters = { list = ["abc", "xyz"], inverted = false }
  copyrights = { list = ["arknights"], inverted = false }
  artists = { list = ["mourncolor", "elodias"], inverted = true }
  tags = { list = ["bird", "horse"], inverted = false }
  ids = { list = ["11112"], inverted = false }
  ratings = { list = ["g"], inverted = true }
  |]

fullSourceParsed :: Sources
fullSourceParsed =
  Sources
    { sources =
        [ ( Source
              { provider = "myprovider"
              , ids = ["12 kok", "13", "14"]
              , overrides =
                  Just
                    [ ( Override
                          { identifier = "kok"
                          , append = True
                          , artists = Nothing
                          , characters = Just ["kokomi"]
                          , copyrights = Just ["genshin_impact"]
                          , tags = Just ["jellyfish", "underwater"]
                          , ratings = Just ["explicit"]
                          }
                      )
                    ]
              , filters =
                  Just
                    Filters
                      { fcharacters = Just Filter{list = ["abc", "xyz"], inverted = False}
                      , fcopyrights = Just Filter{list = ["arknights"], inverted = False}
                      , fartists = Just Filter{list = ["mourncolor", "elodias"], inverted = True}
                      , ftags = Just Filter{list = ["bird", "horse"], inverted = False}
                      , fids = Just Filter{list = ["11112"], inverted = False}
                      , fratings = Just Filter{list = ["g"], inverted = True}
                      }
              , previews =
                  Just
                    Previews
                      { enabled = True
                      , pcharacters = Just Filter{list = ["abc", "xyz"], inverted = False}
                      , pcopyrights = Just Filter{list = ["arknights"], inverted = False}
                      , partists = Just Filter{list = ["mourncolor", "elodias"], inverted = True}
                      , ptags = Just Filter{list = ["bird", "horse"], inverted = False}
                      , pids = Just Filter{list = ["11112"], inverted = False}
                      , pratings = Just Filter{list = ["g"], inverted = True}
                      }
              }
          )
        ]
    }

emptySource :: Text
emptySource =
  [quoteStr|
  [[sources]]
  provider = "danbooru"
  ids = ["834871"]
  |]

emptySourceParsed :: Sources
emptySourceParsed =
  Sources
    { sources =
        [ ( Source
              { provider = "danbooru"
              , ids = ["834871"]
              , overrides = Nothing
              , filters = Nothing
              , previews = Nothing
              }
          )
        ]
    }

spec :: Spec
spec = do
  it "parses exhaustive Source" $ decode fullSource `shouldBe` Success [] fullSourceParsed
  it "parses empty source" $ decode emptySource `shouldBe` Success [] emptySourceParsed
