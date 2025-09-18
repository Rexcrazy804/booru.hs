{-# LANGUAGE DuplicateRecordFields #-}

module Booru.Synonyms (
  NormalSynoMap,
  Synonyms' (..),
  realizeSynonyms,
  toSynonyms',
  replaceSynonyms,
) where

import Booru.Schema.Images (Image)
import qualified Booru.Schema.Images as Img
import Booru.Schema.Synonyms (SynoMap, Synonyms)
import qualified Booru.Schema.Synonyms as Syn
import Data.Map (Map, empty, findWithDefault, foldrWithKey, insert)
import Data.Maybe (fromMaybe)

{- |
A synoMap would be like so:
"kokomi" = { "sango", "koko", "sangonomiya" }

A normal synoMap would however be like so:
"sango" = "kokomi"
"koko" = "kokomi"
"sangonomiya" = "kokomi"
-}
type NormalSynoMap = Map String String

data Synonyms' = Synonyms'
  { artists :: NormalSynoMap
  , characters :: NormalSynoMap
  , copyrights :: NormalSynoMap
  , ratings :: NormalSynoMap
  , tags :: NormalSynoMap
  }
  deriving (Show, Eq)

realizeSynonyms :: Synonyms -> [Image] -> [Image]
realizeSynonyms syns img =
  let syns' = toSynonyms' syns
  in  map (replaceSynonyms syns') img

toSynonyms' :: Synonyms -> Synonyms'
toSynonyms' syns =
  Synonyms'
    { artists = aux $ Syn.artists syns
    , characters = aux $ Syn.characters syns
    , copyrights = aux $ Syn.copyrights syns
    , ratings = aux $ Syn.ratings syns
    , tags = aux $ Syn.tags syns
    }
 where
  aux = normalizeSynoMap . fromMaybe empty

normalizeSynoMap :: SynoMap -> NormalSynoMap
normalizeSynoMap = foldrWithKey aux empty
 where
  aux :: String -> [String] -> NormalSynoMap -> NormalSynoMap
  aux key val nmap = foldr (`insert` key) nmap val

replaceSynonyms :: Synonyms' -> Image -> Image
replaceSynonyms syns' img =
  img
    { Img.artists = mapSyns (artists syns') $ Img.artists img
    , Img.characters = mapSyns (characters syns') $ Img.characters img
    , Img.copyrights = mapSyns (copyrights syns') $ Img.copyrights img
    , Img.rating = unwords $ mapSyns (ratings syns') [Img.rating img]
    , Img.tags = mapSyns (tags syns') $ Img.tags img
    }
 where
  mapSyns :: NormalSynoMap -> [String] -> [String]
  mapSyns synmap = map (\x -> findWithDefault x x synmap)
