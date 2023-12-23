{-# LANGUAGE OverloadedStrings #-}
module Day07 (parser, solutions) where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Bifunctor (first)
import Data.Hashable
import Data.HashMap.Strict qualified as M
import Data.List
import Data.Ord
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

-----------
-- types --
-----------

data Card = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
  deriving (Eq, Ord, Generic)

instance Hashable Card

instance Show Card where
  show card =
    case card of
      Two   -> "2"
      Three -> "3"
      Four  -> "4"
      Five  -> "5"
      Six   -> "6"
      Seven -> "7"
      Eight -> "8"
      Nine  -> "9"
      Ten   -> "T"
      Jack  -> "J"
      Queen -> "Q"
      King  -> "K"
      Ace   -> "A"

data Hand = Hand Card Card Card Card Card
  deriving (Eq, Ord)

instance Show Hand where
  show (Hand c1 c2 c3 c4 c5) =
    "Hand " <> concatMap show [c1, c2, c3, c4, c5]

-------------
-- parsing --
-------------

type Parser = P.Parsec Text Text

parseCard :: Parser Card
parseCard = P.string "2" $> Two
        <|> P.string "3" $> Three
        <|> P.string "4" $> Four
        <|> P.string "5" $> Five
        <|> P.string "6" $> Six
        <|> P.string "7" $> Seven
        <|> P.string "8" $> Eight
        <|> P.string "9" $> Nine
        <|> P.string "T" $> Ten
        <|> P.string "J" $> Jack
        <|> P.string "Q" $> Queen
        <|> P.string "K" $> King
        <|> P.string "A" $> Ace

parseGame :: Parser (Hand, Int)
parseGame = do
  [c1, c2, c3, c4, c5] <- replicateM 5 parseCard
  _ <- P.hspace
  bid <- L.decimal
  pure (Hand c1 c2 c3 c4 c5, bid)

parseGames :: Parser [(Hand, Int)]
parseGames = parseGame `P.sepEndBy1` P.newline

------------
-- part a --
------------

data HandType = HighCard
              | Pair
              | TwoPair
              | ThreeOfAKind
              | FullHouse
              | FourOfAKind
              | FiveOfAKind
  deriving (Eq, Ord, Show)

handType' :: [Int] -> HandType
handType' cardCounts =
  case length cardCounts of
    1 -> FiveOfAKind
    2 -> case product cardCounts of
           4 -> {- 1 * 4 -} FourOfAKind
           6 -> {- 2 * 3 -} FullHouse
           _ -> impossible
    3 -> case product cardCounts of
           3 -> {- 1 * 1 * 3 -} ThreeOfAKind
           4 -> {- 1 * 2 * 2 -} TwoPair
           _ -> impossible
    4 -> Pair
    5 -> HighCard
    _ -> impossible
  where impossible = error "impossible"

handType :: Hand -> HandType
handType (Hand c1 c2 c3 c4 c5) =
  handType' $ M.elems $ M.fromListWith (+) $ map (,1) [c1, c2, c3, c4, c5]

solveA :: [(Hand, Int)] -> Int
solveA games =
  let sorted = sortOn (\(hand, _) -> (handType hand, hand)) games in
  let ranked = zip [1..] sorted in
  sum [rank * bid | (rank, (_, bid)) <- ranked]

------------
-- part b --
------------

data CardB = Joker
           | BTwo
           | BThree
           | BFour
           | BFive
           | BSix
           | BSeven
           | BEight
           | BNine
           | BTen
           | BQueen
           | BKing
           | BAce
  deriving (Eq, Ord, Generic)

instance Hashable CardB

instance Show CardB where
  show card =
    case card of
      Joker  -> "J"
      BTwo   -> "2"
      BThree -> "3"
      BFour  -> "4"
      BFive  -> "5"
      BSix   -> "6"
      BSeven -> "7"
      BEight -> "8"
      BNine  -> "9"
      BTen   -> "T"
      BQueen -> "Q"
      BKing  -> "K"
      BAce   -> "A"

data HandB = HandB CardB CardB CardB CardB CardB
  deriving (Eq, Ord)

instance Show HandB where
  show (HandB c1 c2 c3 c4 c5) =
    "HandB " <> concatMap show [c1, c2, c3, c4, c5]

toBCard :: Card -> CardB
toBCard card =
  case card of
    Two   -> BTwo
    Three -> BThree
    Four  -> BFour
    Five  -> BFive
    Six   -> BSix
    Seven -> BSeven
    Eight -> BEight
    Nine  -> BNine
    Ten   -> BTen
    Jack  -> Joker
    Queen -> BQueen
    King  -> BKing
    Ace   -> BAce

toBHand :: Hand -> HandB
toBHand (Hand c1 c2 c3 c4 c5) =
  HandB (toBCard c1)
        (toBCard c2)
        (toBCard c3)
        (toBCard c4)
        (toBCard c5)

handTypeB :: HandB -> HandType
handTypeB (HandB c1 c2 c3 c4 c5) = handType' cardCountsWJokers
  where
    cards = [c1, c2, c3, c4, c5]
    (numJokers, nonJokers) = first length $ partition (== Joker) cards
    cardCounts = M.elems $ M.fromListWith (+) $ map (,1) nonJokers
    cardCountsWJokers =
      case sortBy (comparing Down) cardCounts of
        [] -> [numJokers]
        (most : rest) -> most + numJokers : rest

solveB :: [(Hand, Int)] -> Int
solveB games =
  sum [rank * bid | (rank, (_, bid)) <- ranked]
  where
    gamesB = map (first toBHand) games
    sorted = sortOn (\(hand, _) -> (handTypeB hand, hand)) gamesB
    ranked = zip [1..] sorted

--------------
-- exported --
--------------

type ParsedStructure = [(Hand, Int)]

parser :: Parser ParsedStructure
parser = parseGames

solutions :: [ParsedStructure -> Text]
solutions = (showOutput .) <$> [solveA, solveB]
  where
    showOutput :: Show a => a -> Text
    showOutput = T.pack . show
