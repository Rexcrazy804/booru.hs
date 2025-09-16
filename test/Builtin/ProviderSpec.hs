{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Builtin.ProviderSpec (spec) where

import Booru.Builtin.Providers
import QuoteStr (quoteStr)
import Test.Hspec (Spec, it, shouldBe)
import Toml (encode)

danbooruDonmaiUsToml :: String
danbooruDonmaiUsToml =
  [quoteStr|
  artists = "tag_string_artist"
  characters = "tag_string_character"
  copyrights = "tag_string_copyright"
  file = "file_url"
  name = "danbooru"
  preview_file = "preview_file_url"
  rating = "rating"
  tags = "tag_string_general"
  url = "https://danbooru.donmai.us/posts/%%ID%%.json"|]

safebooruOrgToml :: String
safebooruOrgToml =
  [quoteStr|
  file = "file_url"
  name = "safebooru"
  preview_file = "sample_url"
  rating = "$general"
  tags = "tags"
  url = "https://safebooru.org/index.php?page=dapi&s=post&q=index&json=1&id=%%ID%%"|]

specialUrlsToml :: String
specialUrlsToml =
  [quoteStr|
  name = "urls"
  url = "%%ID%%"|]

spec :: Spec
spec = do
  it "verifies danbooruDonmaiUs integrity" $ show (encode danbooruDonmaiUs) `shouldBe` danbooruDonmaiUsToml
  it "verifies safebooruOrg integrity" $ show (encode safebooruOrg) `shouldBe` safebooruOrgToml
  it "verifies specialUrls integrity" $ show (encode specialUrls) `shouldBe` specialUrlsToml
