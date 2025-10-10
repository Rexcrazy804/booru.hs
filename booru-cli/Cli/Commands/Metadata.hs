{-# LANGUAGE OverloadedRecordDot #-}

-- yeah this is gonna be a bit of a fucking rant so buckle up,
-- So.... you are telling me, out of fuckign nowhere,
-- HASKELL HAS RECORD DOT SYNTAX???
-- AJSDL KJASL:DK JAL:KSDJ :LKASJDL: AJSD ADSKL:J
-- SOOOOOOOOOOO MUCH CODE THAT WOULD HAVE BEEEN MUUUUUUUCH NICER
-- Hadd I fucking known this earlier
-- FUCKING HELL

module Cli.Commands.Metadata (meta) where

import Booru.Core.Parsers (encode)
import Booru.Schema.Identifier (extractId, extractId')
import Booru.Schema.Images (Image (..), Images (..))
import Cli.Commands.Build (build)
import Cli.Options (BuildOpts (..), CommonOpts (..), MetadataAction (..), MetadataOpts (..))
import Cli.Utils.Common (getData)
import Control.Monad (forM_)
import Data.List (partition)

meta :: MetadataOpts -> CommonOpts -> IO ()
meta mo@MetadataOpts{action = a, resolvedNames = rnames} co@CommonOpts{dataDir = d} = do
  (cachedImgs, datafile, _) <- getData d
  let (reqImgs, otherImgs) = partition ((`elem` rnames) . resolvedName) cachedImgs
  case a of
    Get -> forM_ reqImgs displayMetadata
    Update -> do
      meta mo{action = Remove} co
      build BuildOpts{plantDir = Nothing, skipCat = True} co
    Remove -> do
      forM_ reqImgs (\img -> putStrLn $ "Removing metadata of: " ++ extractId img.id)
      writeFile datafile (show $ encode Images{images = otherImgs})

displayMetadata :: Image -> IO ()
displayMetadata img = do
  putStrLn ""
  putStrLn $ "provider: " ++ img.provider
  putStrLn $ "id: " ++ unwords (extractId' img.id)
  putStrLn $ "file: " ++ img.file
  putStrLn $ "preview_file: " ++ img.preview_file
  putStrLn $ "artists: " ++ unwords img.artists
  putStrLn $ "characters: " ++ unwords img.characters
  putStrLn $ "copyrights: " ++ unwords img.copyrights
  putStrLn $ "rating: " ++ img.rating
  putStrLn $ "tags: " ++ unwords img.tags
