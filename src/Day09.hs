{-# LANGUAGE OverloadedStrings #-}
module Day09 (parser, solutions) where

import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

-----------
-- types --
-----------

-------------
-- parsing --
-------------

type Parser = P.Parsec Text Text

parseLine :: Parser [Int]
parseLine = L.signed P.hspace L.decimal `P.sepBy1` P.hspace

parse :: Parser [[Int]]
parse = parseLine `P.sepEndBy1` P.newline

type ParsedStructure = [[Int]]

------------
-- part a --
------------

diffs :: [Int] -> [Int]
diffs points = zipWith (-) points (tail points)

solveLineA :: [Int] -> Int
solveLineA points = sum $ map head diffTable
  where
    diffTable = flip unfoldr (reverse points) $ \pts ->
      if all (== 0) pts
      then Nothing
      else Just (pts, diffs pts)

solveA :: ParsedStructure -> Int
solveA = sum . map solveLineA

------------
-- part b --
------------

solveB :: ParsedStructure -> Int
solveB = sum . map (solveLineA . reverse)

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
