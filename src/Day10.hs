{-# LANGUAGE OverloadedStrings #-}
module Day10 (parser, solutions) where

import Control.Exception
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Set qualified as S
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

-----------
-- types --
-----------

newtype WorldS = WorldS { chartS :: Vector (Vector Char) }
  deriving Show

-------------
-- parsing --
-------------

type Parser = P.Parsec Text Text

parseCell :: Parser Char
parseCell = P.oneOf @[] "|-LJ7F.S"

parseLine :: Parser (Vector Char)
parseLine = V.fromList <$> P.some parseCell

parse :: Parser WorldS
parse = WorldS . V.fromList <$> (parseLine `P.sepEndBy1` P.newline)

type ParsedStructure = WorldS

------------
-- part a --
------------

data World = World
  { chart :: Vector (Vector Char)
  , startLoc :: (Int, Int)
  }
  deriving Show

worldGet :: World -> (Int, Int) -> Char
worldGet (World chart _) (row, col) = chart V.! row V.! col

-- fixes the pipe and returns start position
worldExtract :: WorldS -> World
worldExtract (WorldS chart) =
  World newContent (startRow, startCol)
  where
    startRow = fromJust $ V.findIndex (elem 'S') chart
    startCol = fromJust $ V.elemIndex 'S' (chart V.! startRow)
    newPipe = '7'  -- TODO: by manual inspection
    newRow = (chart V.! startRow) V.// [(startCol, newPipe)]
    newContent = chart V.// [(startRow, newRow)]

data Direction = North | East | South | West
  deriving (Eq)

nextDirection :: Char -> Direction -> Direction
nextDirection pipe dir =
  case (pipe, dir) of
    ('|', North) -> North
    ('|', South) -> South
    ('-', East) -> East
    ('-', West) -> West
    ('L', West) -> North
    ('L', South) -> East
    ('J', East) -> North
    ('J', South) -> West
    ('7', North) -> West
    ('7', East) -> South
    ('F', North) -> East
    ('F', West) -> South
    _ -> error "should not happen"

directionDiff :: Direction -> (Int, Int) -> (Int, Int)
directionDiff dir (row, col) =
  case dir of
    North -> (row-1, col  )
    East  -> (row  , col+1)
    South -> (row+1, col  )
    West  -> (row  , col-1)

getLoop :: World -> [(Int, Int)]
getLoop world = map fst loop
  where
    startDir = East  -- TODO: by manual inspection

    -- states are (location, direction facing when entering the location)
    start = (startLoc world, startDir)
    nextLoc (loc, dir) =
      let nextDir = nextDirection (worldGet world loc) dir in
      (directionDiff nextDir loc, nextDir)

    path = iterate nextLoc (nextLoc start)
    loop = start : takeWhile (/= start) path

solveA :: ParsedStructure -> Int
solveA world = length (getLoop (worldExtract world)) `div` 2

------------
-- part b --
------------

onlyLoop :: World -> World
onlyLoop world = world {chart = onlyLoopContent}
  where
    loopCells = S.fromList $ getLoop world
    clearNonLoop i c = if i `S.member` loopCells then c else '.'
    onlyLoopContent =
      flip V.imap (chart world) $ \r row ->
        flip V.imap row $ \c cell ->
          clearNonLoop (r, c) cell

data Spin = Up | Down
  deriving Eq

interiorCells :: Vector Char -> Int
interiorCells = go 0 False Nothing . V.toList
  where
    go :: Int -> Bool -> Maybe Spin -> [Char] -> Int
    go acc _        _        []            = acc
    go acc interior pipeSpin (cell : rest) =
      case cell of
        '|' ->
          assert (isNothing pipeSpin) $
          go acc (not interior) pipeSpin rest
        '-' ->
          assert (isJust pipeSpin) $
          assert (not interior) $
          go acc interior pipeSpin rest
        'L' ->
          go acc False (Just (if interior then Down else Up)) rest
        'F' ->
          go acc False (Just (if interior then Up else Down)) rest
        'J' ->
          assert (isJust pipeSpin) $
          go acc (pipeSpin == Just Down) Nothing rest
        '7' ->
          assert (isJust pipeSpin) $
          go acc (pipeSpin == Just Up) Nothing rest
        '.' ->
          assert (isNothing pipeSpin) $
          go (if interior then succ acc else acc) interior pipeSpin rest
        _ ->
          error "impossible"


solveB :: ParsedStructure -> Int
solveB worldS = sum $ V.map interiorCells rows
  where
    world = worldExtract worldS
    rows = chart $ onlyLoop world

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
