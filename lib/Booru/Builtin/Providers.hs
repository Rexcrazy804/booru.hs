{- |
Providers distrobuted by default.
Their Toml equivalants can be found in tests/Builtin/ProviderSpec.hs.
For more detailed information, see examples/providers.toml
-}
module Booru.Builtin.Providers
where

import Booru.Schema.Providers

danbooruDonmaiUs :: Provider
danbooruDonmaiUs =
  Provider
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

safebooruOrg :: Provider
safebooruOrg =
  Provider
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

-- | Special builtin provider accepting an entire url as id
specialUrls :: Provider
specialUrls =
  Provider
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
