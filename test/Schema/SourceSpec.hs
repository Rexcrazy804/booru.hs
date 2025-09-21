{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Schema.SourceSpec (spec) where

import Booru.Schema.Identifier (Identifier (..))
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
  rating = "explicit"
  |]

fullSourceParsed :: Sources
fullSourceParsed =
  Sources
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
  Sources{sources = [(Source{provider = "danbooru", ids = [Id "834871"], overrides = Nothing})]}

multiSource :: Text
multiSource =
  [quoteStr|
  [[sources]]
  provider = "danbooru"
  ids = ["185245", "1876678"]

  [[sources]]
  provider = "danbooru"
  ids = ["8717178", "1678428"]
  |]

multiSourceParsed :: Sources
multiSourceParsed =
  Sources
    { sources =
        [ ( Source
              { provider = "danbooru"
              , ids = [Id "185245", Id "1876678"]
              , overrides = Nothing
              }
          )
        , ( Source
              { provider = "danbooru"
              , ids = [Id "8717178", Id "1678428"]
              , overrides = Nothing
              }
          )
        ]
    }

spec :: Spec
spec = do
  it "parses exhaustive source" $ decode fullSource `shouldBe` Success [] fullSourceParsed
  it "parses empty source" $ decode emptySource `shouldBe` Success [] emptySourceParsed
  it "parses multi sources" $ decode multiSource `shouldBe` Success [] multiSourceParsed
