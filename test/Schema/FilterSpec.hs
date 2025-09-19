{-# LANGUAGE OverloadedStrings #-}

module Schema.FilterSpec (spec) where

import Booru.Schema.Filters
import Data.Text (Text)
import QuoteStr (quoteStr)
import Test.Hspec (Spec, it, shouldBe)
import Toml (Result (..), decode)

fullFilter :: Text
fullFilter =
  [quoteStr|
  characters = { list = ["abc", "xyz"], inverted = false }
  copyrights = { list = ["arknights"], inverted = false }
  artists = { list = ["mourncolor", "elodias"], inverted = true }
  tags = { list = ["bird", "horse"], inverted = false }
  ids = { list = ["11112"], inverted = false }
  ratings = { list = ["g"], inverted = true }
  providers = { list = ["s34"], inverted = false}
  |]

fullFilterParsed :: Filters
fullFilterParsed =
  Filters
    { characters = Just Filter{list = ["abc", "xyz"], inverted = False}
    , copyrights = Just Filter{list = ["arknights"], inverted = False}
    , artists = Just Filter{list = ["mourncolor", "elodias"], inverted = True}
    , tags = Just Filter{list = ["bird", "horse"], inverted = False}
    , ids = Just Filter{list = ["11112"], inverted = False}
    , ratings = Just Filter{list = ["g"], inverted = True}
    , providers = Just Filter{list = ["s34"], inverted = False}
    }

emptyFilter :: Text
emptyFilter =
  [quoteStr|
  |]

emptyFilterParsed :: Filters
emptyFilterParsed =
  Filters
    { characters = Nothing
    , copyrights = Nothing
    , artists = Nothing
    , tags = Nothing
    , ids = Nothing
    , ratings = Nothing
    , providers = Nothing
    }

spec :: Spec
spec = do
  it "parses exhaustive filter" $ decode fullFilter `shouldBe` Success [] fullFilterParsed
  it "parses empty filter" $ decode emptyFilter `shouldBe` Success [] emptyFilterParsed
