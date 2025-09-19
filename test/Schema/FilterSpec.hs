{-# LANGUAGE OverloadedStrings #-}

module Schema.FilterSpec (spec) where

-- NOTE
-- we are only testing Previews since
-- Filters is a subset of Previews
import Booru.Schema.Filters (Filter (..))
import Booru.Schema.PFilters (PFilters (..))
import Data.Text (Text)
import QuoteStr (quoteStr)
import Test.Hspec (Spec, it, shouldBe)
import Toml (Result (..), decode)

fullPreview :: Text
fullPreview =
  [quoteStr|
  characters = { list = ["abc", "xyz"], inverted = false }
  copyrights = { list = ["arknights"], inverted = false }
  artists = { list = ["mourncolor", "elodias"], inverted = true }
  tags = { list = ["bird", "horse"], inverted = false }
  ids = { list = ["11112"], inverted = false }
  ratings = { list = ["g"], inverted = true }
  providers = { list = ["s34"], inverted = false}
  |]

fullPreviewParsed :: PFilters
fullPreviewParsed =
  PFilters
    { characters = Just Filter{list = ["abc", "xyz"], inverted = False}
    , copyrights = Just Filter{list = ["arknights"], inverted = False}
    , artists = Just Filter{list = ["mourncolor", "elodias"], inverted = True}
    , tags = Just Filter{list = ["bird", "horse"], inverted = False}
    , ids = Just Filter{list = ["11112"], inverted = False}
    , ratings = Just Filter{list = ["g"], inverted = True}
    , providers = Just Filter{list = ["s34"], inverted = False}
    }

emptyPreview :: Text
emptyPreview =
  [quoteStr|
  |]

emptyPreviewParsed :: PFilters
emptyPreviewParsed =
  PFilters
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
  it "parses exhaustive filter" $ decode fullPreview `shouldBe` Success [] fullPreviewParsed
  it "parses empty filter" $ decode emptyPreview `shouldBe` Success [] emptyPreviewParsed
