{-# LANGUAGE OverloadedStrings #-}
module Day02 (parse, solutions) where

import Control.Applicative
import Data.Functor
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

-- Types

data Color = Red | Green | Blue
data Blocks = Blocks Color Int

data BlockSet = BlockSet { red :: Int, green :: Int, blue :: Int }
  deriving Show
data Game = Game Int [BlockSet]
  deriving Show

-------------
-- parsing --
-------------

type Parser = P.Parsec Void Text

parseColor :: Parser Color
parseColor =
  P.string "red" $> Red <|>
  P.string "green" $> Green <|>
  P.string "blue" $> Blue

parseBlocks :: Parser Blocks
parseBlocks = do
  n <- L.decimal
  _ <- P.space
  color <- parseColor
  pure $ Blocks color n

parseSubset :: Parser BlockSet
parseSubset = do
  allBlocks <- parseBlocks `P.sepBy` P.string ", "
  pure $ BlockSet { red   = sum [n | Blocks Red n <- allBlocks]
                  , green = sum [n | Blocks Green n <- allBlocks]
                  , blue  = sum [n | Blocks Blue n <- allBlocks]
                  }

parseGame :: Parser Game
parseGame = do
  _ <- P.string "Game "
  n <- L.decimal
  _ <- P.string ": "
  subsets <- parseSubset `P.sepBy` P.string "; "
  _ <- P.newline
  pure $ Game n subsets

parse :: Text -> [Game]
parse = fromJust . P.parseMaybe (many parseGame)

------------
-- part a --
------------

subset :: BlockSet -> BlockSet -> Bool
subset left right = and $
  zipWith (<=) [l_red, l_green, l_blue] [r_red, r_green, r_blue]
  where
    BlockSet {red = l_red, green = l_green, blue = l_blue} = left
    BlockSet {red = r_red, green = r_green, blue = r_blue} = right

maximumA :: BlockSet
maximumA = BlockSet
  { red = 12
  , green = 13
  , blue = 14
  }

solveA :: [Game] -> Int
solveA games = sum
  [ i
  | Game i blockset <- games
  , and [blocks `subset` maximumA | blocks <- blockset]
  ]

------------
-- part b --
------------

-- least upper bound
lub :: BlockSet -> BlockSet -> BlockSet
lub left right = BlockSet
  { red = max l_red r_red
  , green = max l_green r_green
  , blue = max l_blue r_blue
  }
  where
    BlockSet {red = l_red, green = l_green, blue = l_blue} = left
    BlockSet {red = r_red, green = r_green, blue = r_blue} = right

solveB :: [Game] -> Int
solveB games = sum
  [ r * g * b
  | Game _ blocksets <- games
  , let BlockSet r g b = foldr1 lub blocksets
  ]

showOutput :: Int -> Text
showOutput = T.pack . show

solutions :: [[Game] -> Text]
solutions = map (showOutput .) [solveA, solveB]
