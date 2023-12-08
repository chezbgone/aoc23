{-# LANGUAGE OverloadedStrings #-}
module Day01 (solutions) where

import Data.Char (isDigit)
import Data.List (tails, isPrefixOf)
import Data.Maybe (listToMaybe, mapMaybe)

parse :: String -> [String]
parse = lines

solveA :: String -> Int
solveA line = read @Int [head digits, last digits]
  where digits = filter isDigit line

numbers :: [(String, Char)]
numbers = zip
  ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
  ['1'..]

startsWithNumber :: String -> Maybe Char
startsWithNumber xs = listToMaybe [n | (ns, n) <- numbers, ns `isPrefixOf` xs]

isDigitPosition :: String -> Maybe Char
isDigitPosition [] = Nothing
isDigitPosition xs@(x:_)
  | isDigit x = Just x
  | otherwise = startsWithNumber xs

solveB :: String -> Int
solveB line = read @Int [head digits, last digits]
  where
    digits = mapMaybe isDigitPosition $ tails line

byLines :: (String -> Int) -> ([String] -> Int)
byLines solve_line = sum . map solve_line

showOutput :: Int -> String
showOutput = show

solutions :: [String -> String]
solutions = map (\s -> showOutput . s . parse) [byLines solveA, byLines solveB]
