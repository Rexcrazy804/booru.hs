{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Schema.ProviderSpec (spec) where

import Booru.Schema.Providers
import Data.Text (Text)
import QuoteStr (quoteStr)
import Test.Hspec (Spec, it, shouldBe)
import Toml (Result (..), decode)

fullProvider :: Text
fullProvider =
  [quoteStr|
  [[providers]]
  name = "danbooru"
  url = "https://danbooru.donmai.us/posts/%%ID%%.json"
  file = "file_url"
  preview_file = "preview_file_url"
  artists = "tag_string_artist"
  characters = "tag_string_character"
  copyrights = "tag_string_copyright"
  tags = "tag_string_general"
  rating = "rating"
  |]

fullProviderParsed :: Providers
fullProviderParsed =
  Providers
    { providers =
        [ ( Provider
              { name = "danbooru"
              , url = "https://danbooru.donmai.us/posts/%%ID%%.json"
              , file = Just $ Attr ["file_url"]
              , preview_file = Just $ Attr ["preview_file_url"]
              , artists = Just $ Attr ["tag_string_artist"]
              , characters = Just $ Attr ["tag_string_character"]
              , copyrights = Just $ Attr ["tag_string_copyright"]
              , tags = Just $ Attr ["tag_string_general"]
              , rating = Just $ Attr ["rating"]
              }
          )
        ]
    }

partialProvider :: Text
partialProvider =
  [quoteStr|
  [[providers]]
  name = "safebooru"
  url = "https://safebooru.org/index.php?page=dapi&s=post&q=index&json=1&id=%%ID%%"
  file = "file_url"
  preview_file = "sample_url"
  tags = "tags"
  rating = "$general"
  |]

partialProviderParsed :: Providers
partialProviderParsed =
  Providers
    { providers =
        [ ( Provider
              { name = "safebooru"
              , url = "https://safebooru.org/index.php?page=dapi&s=post&q=index&json=1&id=%%ID%%"
              , file = Just $ Attr ["file_url"]
              , preview_file = Just $ Attr ["sample_url"]
              , tags = Just $ Attr ["tags"]
              , artists = Nothing
              , characters = Nothing
              , copyrights = Nothing
              , rating = Just $ Default "general"
              }
          )
        ]
    }

minimalProvider :: Text
minimalProvider =
  [quoteStr|
  [[providers]]
  name = "urls"
  url = "%%ID%%"
  |]

minimalProviderParsed :: Providers
minimalProviderParsed =
  Providers
    { providers =
        [ ( Provider
              { name = "urls"
              , url = "%%ID%%"
              , file = Nothing
              , preview_file = Nothing
              , tags = Nothing
              , artists = Nothing
              , characters = Nothing
              , copyrights = Nothing
              , rating = Nothing
              }
          )
        ]
    }

spec :: Spec
spec = do
  it "parses full provider" $ decode fullProvider `shouldBe` Success [] fullProviderParsed
  it "parses partial provider" $ decode partialProvider `shouldBe` Success [] partialProviderParsed
  it "parses minimal provider" $ decode minimalProvider `shouldBe` Success [] minimalProviderParsed
