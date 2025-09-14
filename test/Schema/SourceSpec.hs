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

  [sources.previews.filters]
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
                      , pfilters =
                          Just
                            Filters
                              { fcharacters = Just Filter{list = ["abc", "xyz"], inverted = False}
                              , fcopyrights = Just Filter{list = ["arknights"], inverted = False}
                              , fartists = Just Filter{list = ["mourncolor", "elodias"], inverted = True}
                              , ftags = Just Filter{list = ["bird", "horse"], inverted = False}
                              , fids = Just Filter{list = ["11112"], inverted = False}
                              , fratings = Just Filter{list = ["g"], inverted = True}
                              }
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

simpleFilter :: Text
simpleFilter =
  [quoteStr|
  ids = { list = ["123"], inverted = false }
  characters = { list = ["barbara"], inverted = true}
  ratings = { list = ["e"], inverted = false }
  |]

simpleFilterParsed :: Filters
simpleFilterParsed =
  Filters
    { fids = Just Filter{list = ["123"], inverted = False}
    , fartists = Nothing
    , fcharacters = Just Filter{list = ["barbara"], inverted = True}
    , fcopyrights = Nothing
    , ftags = Nothing
    , fratings = Just Filter{list = ["e"], inverted = False}
    }

simplePreviews :: Text
simplePreviews =
  [quoteStr|
  enabled = true
  [filters]
  artists = { list = ["elodias"], inverted = true }
  copyrights = { list = ["genshin_impact"], inverted = true}
  tags = { list = ["birds"], inverted = false }
  |]

simplePreviewsParsed :: Previews
simplePreviewsParsed =
  Previews
    { enabled = True
    , pfilters =
        Just
          Filters
            { fids = Nothing
            , fartists = Just Filter{list = ["elodias"], inverted = True}
            , fcharacters = Nothing
            , fcopyrights = Just Filter{list = ["genshin_impact"], inverted = True}
            , ftags = Just Filter{list = ["birds"], inverted = False}
            , fratings = Nothing
            }
    }

multiSource :: Text
multiSource =
  [quoteStr|
  [[sources]]
  provider = "danbooru"
  ids = ["185245", "1876678"]
  [sources.filters]
  characters = { list = ["kokomi"], inverted = true }
  [sources.previews]
  enabled = true
  [sources.previews.filters]
  ratings = { list = ["g"], inverted = true }

  [[sources]]
  provider = "danbooru"
  ids = ["8717178", "1678428"]
  [sources.filters]
  copyrights = { list = ["nier_automata"], inverted = true }
  [sources.previews]
  enabled = true
  [sources.previews.filters]
  ratings = { list = ["e"], inverted = false }
  |]

multiSourceParsed :: Sources
multiSourceParsed =
  Sources
    { sources =
        [ ( Source
              { provider = "danbooru"
              , ids = ["185245", "1876678"]
              , overrides = Nothing
              , filters =
                  Just
                    Filters
                      { fcharacters = Just Filter{list = ["kokomi"], inverted = True}
                      , fcopyrights = Nothing
                      , fids = Nothing
                      , ftags = Nothing
                      , fartists = Nothing
                      , fratings = Nothing
                      }
              , previews =
                  Just
                    Previews
                      { enabled = True
                      , pfilters =
                          Just
                            Filters
                              { fratings = Just Filter{list = ["g"], inverted = True}
                              , fcopyrights = Nothing
                              , fids = Nothing
                              , ftags = Nothing
                              , fartists = Nothing
                              , fcharacters = Nothing
                              }
                      }
              }
          )
        , ( Source
              { provider = "danbooru"
              , ids = ["8717178", "1678428"]
              , overrides = Nothing
              , filters =
                  Just
                    Filters
                      { fcopyrights = Just Filter{list = ["nier_automata"], inverted = True}
                      , fcharacters = Nothing
                      , fids = Nothing
                      , ftags = Nothing
                      , fartists = Nothing
                      , fratings = Nothing
                      }
              , previews =
                  Just
                    Previews
                      { enabled = True
                      , pfilters =
                          Just
                            Filters
                              { fratings = Just Filter{list = ["e"], inverted = False}
                              , fcopyrights = Nothing
                              , fids = Nothing
                              , ftags = Nothing
                              , fartists = Nothing
                              , fcharacters = Nothing
                              }
                      }
              }
          )
        ]
    }

spec :: Spec
spec = do
  it "parses exhaustive Source" $ decode fullSource `shouldBe` Success [] fullSourceParsed
  it "parses empty source" $ decode emptySource `shouldBe` Success [] emptySourceParsed
  it "parses simple filter" $ decode simpleFilter `shouldBe` Success [] simpleFilterParsed
  it "parses simple previews" $ decode simplePreviews `shouldBe` Success [] simplePreviewsParsed
  it "parses multi sources" $ decode multiSource `shouldBe` Success [] multiSourceParsed
