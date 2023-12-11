{-# LANGUAGE OverloadedStrings #-}
module Day03 (parser, solutions) where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Data.Char (isDigit)

-----------
-- types --
-----------

data Cell = Empty | Digit Char | Symbol Char
  deriving (Eq, Show)
type Dimensions = (Int, Int)
data Grid = Grid Dimensions (Vector (Vector Cell))
  deriving Show

-------------
-- parsing --
-------------

type Parser = P.Parsec Void Text

parseCell :: Parser Cell
parseCell = charToCell <$> P.anySingleBut '\n'
  where
    charToCell '.' = Empty
    charToCell c | isDigit c = Digit c
    charToCell c = Symbol c

parseRow :: Parser (Vector Cell)
parseRow = do
  cells <- many parseCell
  _ <- P.newline
  pure $ V.fromList cells

parseGrid :: Parser Grid
parseGrid = do
  rows <- many parseRow
  case map V.length rows of
    [] -> P.failure Nothing S.empty
    (l:ls) -> do
      when (any (/= l) ls) $ P.failure Nothing S.empty
      let len = length rows
      pure $ Grid (len, l) (V.fromList rows)

------------
-- part a --
------------

cellIsDigit :: Cell -> Bool
cellIsDigit (Digit _) = True
cellIsDigit _ = False

cellIsSymbol :: Cell -> Bool
cellIsSymbol (Symbol _) = True
cellIsSymbol _ = False

--                               i    len  num
numbersInRow :: Vector Cell -> [(Int, Int, Int)]
numbersInRow row =
  case digit_groups of
    []    -> []
    (e:_) ->
      case V.head e of
        (_, Digit _) -> map digit_group_to_res $ every_other digit_groups
        _            -> map digit_group_to_res $ every_other $ drop 1 digit_groups
  where
    elems = V.indexed row
    digit_groups = V.groupBy ((==) `on` (cellIsDigit . snd)) elems

    num_of_digits digits = read @Int $ map unDigit digits
      where
        unDigit (Digit d) = d
        unDigit _ = error "num_of_digits: not a digit"

    digit_group_to_res :: Vector (Int, Cell) -> (Int, Int, Int)
    digit_group_to_res digits =
      case V.toList digits of
        [] -> error "digit_group_to_res: digit group empty"
        digit_group@((i, _) : _) ->
          (i, length digit_group, num_of_digits (map snd digit_group))

    every_other [] = []
    every_other [x] = [x]
    every_other (x:_:rest) = x : every_other rest

adjacentCells
  :: (Int, Int)
  -> (Int, Int, Int)
  -> [(Int, Int)]
adjacentCells (rows, cols) (row, col, len) =
  filter (\(r, c) -> 0 <= r && r < rows && 0 <= c && c < cols) $
    [ (pred row, pred col)
    , (     row, pred col)
    , (succ row, pred col)
    ] <>  -- left
    [ (pred row, col + len)
    , (     row, col + len)
    , (succ row, col + len)
    ]  -- right
    <> [(pred row, col + i) | i <- [0 .. len-1]]  -- above
    <> [(succ row, col + i) | i <- [0 .. len-1]]  -- below

--                     row  col  len  num
numbersIn :: Grid -> [(Int, Int, Int, Int)]
numbersIn (Grid _ grid) =
  [ (row, col, len, num)
  | (row, rowData) <- zip [0..] $ V.toList grid
  , (col, len, num) <- numbersInRow rowData
  ]

solveA :: Grid -> Int
solveA grid@(Grid dims gridData) =
  sum numbersWithAdjacentSymbols
  where
    numbers = numbersIn grid
    numbersWithAdjacentSymbols =
      [ num
      | (row, col, len, num) <- numbers
      , let numAdjacentCells = adjacentCells dims (row, col, len)
      , any (\(r, c) -> cellIsSymbol (gridData V.! r V.! c)) numAdjacentCells
      ]

------------
-- part b --
------------

solveB :: Grid -> Int
solveB grid@(Grid dims@(rows, cols) gridData) = gears
  where
    numbers = numbersIn grid

    numberMask :: Vector (Vector (Maybe (Int, Int)))
    numberMask = V.create $ do
      g <- MV.replicateM rows $
        MV.replicate cols Nothing
      let write2 (row, col) a = do
            g_row <- MV.read g row
            MV.write g_row col $ Just a
            pure ()
      forM_ (zip [0..] numbers) $ \(i, (row, col, len, n)) ->
        forM_ [col .. col+len-1] $ \col' ->
          write2 (row, col') (i, n)
      g_frozen <- V.unsafeFreeze g
      v <- V.mapM V.unsafeFreeze g_frozen
      V.unsafeThaw v

    adjacent_cells (row, col) = adjacentCells dims (row, col, 1)

    stars =
      [ (row, col)
      | row <- [0 .. rows-1]
      , col <- [0 .. cols-1]
      , gridData V.! row V.! col == Symbol '*'
      ]
    gear_nums coord = S.toList adj_numbers
      where
        adj_numbers = S.fromList
          [ n
          | (r, c) <- adjacent_cells coord
          , Just (_, n) <- [numberMask V.! r V.! c]
          ]
    gears =
      sum $
        map product $
          filter (\g_nums -> length g_nums == 2) $
            map gear_nums stars

showOutput :: Show a => a -> Text
showOutput = T.pack . show

--------------
-- exported --
--------------

parser :: Parser Grid
parser = parseGrid

solutions :: [Grid -> Text]
solutions = map (showOutput .) [solveA, solveB]
