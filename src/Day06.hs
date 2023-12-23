{-# LANGUAGE OverloadedStrings #-}
module Day06 (parser, solutions) where

import Control.Monad
import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

-----------
-- types --
-----------

data Race = Race
  { time :: Int
  , distance :: Int
  }
  deriving Show

newtype Races = Races [Race]
  deriving Show

-------------
-- parsing --
-------------

type Parser = P.Parsec Text Text

parseRaces :: Parser Races
parseRaces = do
  _ <- P.string "Time:" >> P.hspace
  times <- L.decimal `P.sepBy` P.hspace
  _ <- P.newline
  _ <- P.string "Distance:" >> P.hspace
  distances <- L.decimal `P.sepBy` P.hspace
  _ <- P.newline
  when (length times /= length distances) $
    P.customFailure "Different number of values for times and distances"
  pure $ Races $ zipWith Race times distances

------------
-- part a --
------------

waysToBeat :: Race -> Int
waysToBeat (Race time record) =
  case find beats_record [1 .. time `div` 2] of
    Nothing -> 0
    Just b  -> time - 2 * b + 1
  where
    beats_record t = t * (time - t) > record

solveA :: Races -> Int
solveA (Races races) = product $ map waysToBeat races

------------
-- part b --
------------

concatNumbers :: [Int] -> Int
concatNumbers = read . concatMap show

solveB :: Races -> Int
solveB (Races races) =
  let t = concatNumbers $ map time races in
  let d = concatNumbers $ map distance races in
  waysToBeat (Race t d)

--------------
-- exported --
--------------

type ParsedStructure = Races

parser :: Parser ParsedStructure
parser = parseRaces

solutions :: [ParsedStructure -> Text]
solutions = (showOutput .) <$> [solveA, solveB]
  where
    showOutput :: Show a => a -> Text
    showOutput = T.pack . show
