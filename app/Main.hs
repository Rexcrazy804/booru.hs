{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (Value)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest, setRequestHeader)

main :: IO ()
main = do
  request' <- parseRequest "https://danbooru.donmai.us/posts/9881008.json"
  let request = setRequestHeader "User-Agent" ["curl/8.14.1"] request'
  response <- httpJSON request
  print (getResponseBody response :: Maybe Value)
