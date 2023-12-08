{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Data.String ( IsString(..) )
import Data.Text (Text)
import Data.Text qualified as T
import Data.ByteString.Char8 qualified as B
import System.Directory (doesPathExist)
import System.Environment qualified as Env

import Configuration.Dotenv qualified as Env
import Network.HTTP.Simple (parseRequest, httpBS, getResponseBody, addRequestHeader)
import Text.Printf (printf)
import Data.Functor ((<&>))

import Day02 qualified

class IsString a => StringLike a where
  toString :: a -> String

instance StringLike String where
  toString = id

instance StringLike Text where
  toString = T.unpack

getInput :: Int -> IO ()
getInput day = do
  Env.loadFile Env.defaultConfig
  token <- Env.getEnv "token"

  let url = "https://adventofcode.com/2023/day/" <> show day <> "/input"
  request <- parseRequest url <&> addRequestHeader "cookie" (B.pack token)
  response <- getResponseBody <$> httpBS request
  B.writeFile ("input/day" <> printf "%02d" day) response

solve :: (StringLike a, StringLike b) => String -> [a -> b] -> IO ()
solve file solutions = do
  contents <- fromString <$> readFile file
  mapM_ putStrLn [toString (sol contents) | sol <- solutions]

main :: IO ()
main = do
  let day = 2  -- TODO: change this
  let input = "input/day" <> printf "%02d" day
  cached <- doesPathExist input
  unless cached $ do
    putStrLn (input <> " not cached. requesting.")
    getInput day
  solve input Day02.solutions -- TODO: change this
