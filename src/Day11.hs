{-# LANGUAGE OverloadedStrings #-}
module Day11 (parser, solutions) where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.List
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

-----------
-- types --
-----------

type Galaxy = Vector (Vector Bool)

-------------
-- parsing --
-------------

type Parser = P.Parsec Text Text

parseCell :: Parser Bool
parseCell = P.string "." $> False
        <|> P.string "#" $> True

parseLine :: Parser (Vector Bool)
parseLine = V.fromList <$> P.some parseCell

parse :: Parser Galaxy
parse = V.fromList <$> parseLine `P.sepEndBy1` P.newline

type ParsedStructure = Galaxy

------------
-- part a --
------------

expandVertically :: Galaxy -> Galaxy
expandVertically chart = do
  row <- chart
  if not (or row)
  then V.fromList [row, row]
  else V.singleton row

transposeV :: Vector (Vector a) -> Vector (Vector a)
transposeV = V.fromList . map V.fromList . transpose . map V.toList . V.toList

expandGalaxy :: Galaxy -> Galaxy
expandGalaxy = expandVertically . transposeV . expandVertically . transposeV

galaxyLocations :: Galaxy -> [(Int, Int)]
galaxyLocations chart = do
  (r, row) <- zip [0..] $ V.toList chart
  (c, cell) <- zip [0..] $ V.toList row
  guard cell
  pure (r, c)

taxicabDist :: (Int, Int) -> (Int, Int) -> Int
taxicabDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

pairwise :: (a -> a -> b) -> [a] -> [b]
pairwise _ [] = []
pairwise f (x : xs) = map (f x) xs <> pairwise f xs

solveA :: ParsedStructure -> Int
solveA chart =
  sum $ pairwise taxicabDist $ galaxyLocations $ expandGalaxy chart

------------
-- part b --
------------

expandedRows :: Galaxy -> IntSet
expandedRows chart = IS.fromList
  [ i
  | (i, row) <- zip [0..] $ V.toList chart
  , not (or row)
  ]

expandedRowsCols :: Galaxy -> (IntSet, IntSet)
expandedRowsCols chart = (expandedRows chart, expandedRows (transposeV chart))

betweenIS :: Int -> Int -> IntSet -> IntSet
betweenIS l r s = s''
  where
    (_, s') = IS.split l s
    (s'', _) = IS.split r s'

taxicabWithExpanded
  :: (IntSet, IntSet)
  -> Int
  -> (Int, Int)
  -> (Int, Int)
  -> Int
taxicabWithExpanded (eRows, eCols) scale (x1, y1) (x2, y2) =
  abs (x1 - x2) + abs (y1 - y2) +
    (scale - 1) * IS.size (betweenIS lx rx eRows) +
    (scale - 1) * IS.size (betweenIS ly ry eCols)
  where
    lx = min x1 x2
    ly = min y1 y2
    rx = max x1 x2
    ry = max y1 y2

solveB :: ParsedStructure -> Int
solveB chart =
  sum $ pairwise (taxicabWithExpanded eRC scale) $ galaxyLocations chart
  where
    eRC = expandedRowsCols chart
    scale = 1_000_000

--------------
-- exported --
--------------

parser :: Parser ParsedStructure
parser = parse

solutions :: [ParsedStructure -> Text]
solutions = (showOutput .) <$> [solveA, solveB]
  where
    showOutput :: Show a => a -> Text
    showOutput = T.pack . show
