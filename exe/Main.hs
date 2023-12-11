{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Control.Monad
import Data.ByteString.Char8 qualified as B
import Data.Functor
import Data.String ( IsString(..) )
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import System.Directory (doesPathExist)
import System.Environment qualified as Env
import Text.Printf (printf)

import Configuration.Dotenv qualified as Env
import Network.HTTP.Simple (parseRequest, httpBS, getResponseBody, addRequestHeader)
import Text.Megaparsec qualified as P

import Day04 (parser, solutions) -- TODO: change these
aocDay :: Int
aocDay = 4

class IsString a => Stringable a where
  toString :: a -> String

instance Stringable String where
  toString = id

instance Stringable Text where
  toString = T.unpack

getInput :: Int -> IO ()
getInput day = do
  Env.loadFile Env.defaultConfig
  token <- Env.getEnv "token"
  let url = "https://adventofcode.com/2023/day/" <> show day <> "/input"
  request <- parseRequest url <&> addRequestHeader "cookie" (B.pack token)
  response <- getResponseBody <$> httpBS request
  B.writeFile ("input/day" <> printf "%02d" day) response

type Parser = P.Parsec Void Text

parseSolve
  :: Stringable b
  => Parser a
  -> FilePath
  -> [a -> b]
  -> IO ()
parseSolve p file sols = do
  input <- fromString <$> readFile file
  case P.parse (p <* P.eof) "" input of
    Left err      -> putStrLn $ P.errorBundlePretty err
    Right problem -> forM_ sols $ \sol ->
      putStrLn $ toString (sol problem)

main :: IO ()
main = do
  let file = "input/day" <> printf "%02d" aocDay
  cached <- doesPathExist file
  unless cached $ do
    putStrLn (file <> " not cached. requesting.")
    getInput aocDay
  parseSolve parser file solutions
