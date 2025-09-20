module Main where

import Booru.Builtin.Providers (danbooruDonmaiUs)
import Booru.Core.Requests (requestFile, resolveProvider)
import Booru.Schema.Images (
  Identifier (Id),
  Image (Image, resolvedName),
  resolvedName,
 )
import Control.Monad (forM, forM_)
import qualified Data.ByteString as L
import Data.Maybe (catMaybes)
import Options.Applicative
import System.Environment (getArgs)

data Sample = Sample
  { hello :: String
  , quiet :: Bool
  , segs :: Int
  }

newtype Options = Options {cmd :: Commands}

data Commands = Get Sample | Put Sample

getCommand :: Parser Commands
getCommand = Get <$> sample

putCommand :: Parser Commands
putCommand = Put <$> sample

sample :: Parser Sample
sample =
  Sample
    <$> strOption
      ( long "hello"
          <> metavar "TARGET"
          <> help "Target for printing"
      )
    <*> switch
      ( long "quiet"
          <> short 'q'
          <> help "whether to shut up"
      )
    <*> option
      auto
      ( long "segs"
          <> short 's'
          <> help "how many time to print exclaimation"
          <> showDefault
          <> value 1
          <> metavar "INT"
      )

main :: IO ()
main = bob =<< execParser opts
 where
  opts =
    info
      (commands <**> helper)
      ( fullDesc
          <> progDesc "Print a greeting for TARGET"
          <> header "hello - a test for optparse-applicative"
      )
  commands =
    Options
      <$> hsubparser
        ( command "get" (info getCommand (progDesc "get some data"))
            <> command "put" (info putCommand (progDesc "put some data"))
        )

bob :: Options -> IO ()
bob Options{cmd = Get x} = greet x
bob Options{cmd = Put x} = greet x

greet :: Sample -> IO ()
greet (Sample h True n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet (Sample h False n) = do
  putStrLn "printing hello"
  putStrLn $ "Hello, " ++ h ++ "!" ++ replicate n '!'

main' :: IO ()
main' = do
  ids <- getArgs
  let booruFetcher = resolveProvider danbooruDonmaiUs
      idnfrs = map Id ids
  imgs' <- forM idnfrs booruFetcher
  let imgs = catMaybes imgs'
  forM_ imgs downloadImage

downloadImage :: Image -> IO ()
downloadImage img@Image{resolvedName = name} = do
  rsbod <- requestFile img
  L.writeFile name rsbod
  return ()
