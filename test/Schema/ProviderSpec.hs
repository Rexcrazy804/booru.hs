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
  name = "safebooru"
  url = "https://safebooru.org/index.php?page=dapi&s=post&q=index&json=1&id=%%ID%%"
  file = "file_url"
  preview_file = "sample_url"
  tags = "tags"
  |]

-- [[providers]]
-- name = "urls"
-- url = "%%ID%%"
--
-- [[providers]]
-- name = "danbooru"
-- url = "https://danbooru.donmai.us/posts/%%ID%%.json"
-- file = "file_url"
-- preview_file = "preview_file_url"
-- artists = "tag_string_artist"
-- characters = "tag_string_character"
-- copyrights = "tag_string_copyright"
-- tags = "tag_string_general"
-- rating = "rating"

fullProviderParsed :: Providers
fullProviderParsed =
  Providers
    { providers =
        [ ( Provider
              { name = "safebooru"
              , url = "https://safebooru.org/index.php?page=dapi&s=post&q=index&json=1&id=%%ID%%"
              , file = Just "file_url"
              , preview_file = Just "sample_url"
              , tags = Just "tags"
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
  it "parses exhaustive Source" $ decode fullProvider `shouldBe` Success [] fullProviderParsed
