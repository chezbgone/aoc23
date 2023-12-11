{-# LANGUAGE OverloadedStrings #-}
module Day04 (parser, solutions) where

import Control.Monad.State
import Data.Void (Void)
import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

-----------
-- types --
-----------

data Card = Card Int [Int] [Int]
  deriving Show

-------------
-- parsing --
-------------

type Parser = P.Parsec Void Text

parseCard :: Parser Card
parseCard = do
  _ <- P.string "Card" >> P.space
  i <- L.decimal
  _ <- P.string ":" >> P.space
  ls <- L.decimal `P.sepEndBy` P.space
  _ <- P.string "|" >> P.space
  rs <- L.decimal `P.sepBy` P.hspace
  pure $ Card i ls rs

------------
-- part a --
------------

cardMatches :: Card -> Int
cardMatches (Card _ winners ns) = length $ winners `intersect` ns

cardPoints :: Card -> Int
cardPoints = pointFunction . cardMatches
  where
    pointFunction :: Int -> Int
    pointFunction 0 = 0
    pointFunction n = 2 ^ (n - 1)

solveA :: [Card] -> Int
solveA = sum . map cardPoints

------------
-- part b --
------------

solveB :: [Card] -> Int
solveB cards =
  evalState go ([map (const 1) cards], 0)
  where
    pointss = map cardMatches cards
    -- ([[Int]], Int) at card k represents (
    --   how many cards we got from previous cards,
    --   accumuated number of cards used
    -- )
    -- e.g. at card 3,
    -- [[1, 1, 1, 1], [4, 4], [2, 2, 2]]
    -- means we have 1+4+2 card3s,
    --               1+4+2 card4s,
    --               1  +2 card5s,
    --           and 1     card6s.
    go :: State ([[Int]], Int) Int
    go = do
      forM_ pointss $ \points -> do
        (cardPiles, acc) <- get
        let numCards = sum $ map head cardPiles
        let remainingCards = filter (not . null) $ map tail cardPiles
        let newCards =
              case points of
                0 -> remainingCards
                _ -> replicate points numCards : remainingCards
        put (newCards, acc + numCards)
      (_, acc) <- get
      pure acc

--------------
-- exported --
--------------

parser :: Parser [Card]
parser = parseCard `P.sepEndBy` P.newline

solutions :: [[Card] -> Text]
solutions = map (showOutput .) [solveA, solveB]
  where showOutput = T.pack . show
