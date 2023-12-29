{-# LANGUAGE OverloadedStrings #-}
module Day12 (parser, solutions) where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

import Debug.Trace
import Data.Maybe

-----------
-- types --
-----------

data Status = Operational | Damaged

class PShow p where
  pshow :: p -> String

instance PShow Status where
  pshow status =
    case status of
      Operational -> "."
      Damaged -> "#"

instance PShow (Maybe Status) where
  pshow = maybe "?" pshow

instance PShow p => PShow [p] where
  pshow = concatMap pshow

-------------
-- parsing --
-------------

type Parser = P.Parsec Text Text

parseStatus :: Parser (Maybe Status)
parseStatus = P.string "." $> Just Operational
          <|> P.string "#" $> Just Damaged
          <|> P.string "?" $> Nothing

parseLine :: Parser ([Maybe Status], [Int])
parseLine = do
  wells <- P.some parseStatus
  _ <- P.string " "
  damagedCounts <- L.decimal `P.sepBy1` P.string ","
  pure (wells, damagedCounts)

parse :: Parser [([Maybe Status], [Int])]
parse = parseLine `P.sepEndBy1` P.newline

type ParsedStructure = [([Maybe Status], [Int])]

------------
-- part a --
------------

wellConfigs''' :: [Int] -> Int -> [[Status]]
wellConfigs''' _ total
  | total < 0 = []
wellConfigs''' [] total = [replicate total Operational]
wellConfigs''' [damaged] total
  | damaged == total = [replicate damaged Damaged]
wellConfigs''' damagedCounts@(damaged : drest) total = (do
    rest <- wellConfigs''' damagedCounts (total - 1)
    pure $ Operational : rest
  ) <> (do
    rest <- wellConfigs''' drest (total - damaged - 1)
    pure $ replicate damaged Damaged <> (Operational : rest)
  )

unifiable :: [Maybe Status] -> [Status] -> Bool
unifiable ms ss = and $ zipWith unifiable' ms_suffix ss
  where
    ms_suffix = drop (length ms - length ss) ms
    unifiable' Nothing _ = True
    unifiable' (Just Operational) Operational = True
    unifiable' (Just Damaged) Damaged = True
    unifiable' _ _ = False

solveLineA :: [Maybe Status] -> [Int] -> Int
solveLineA statusesM damagedCounts = length $ do
  wellStatuses <- wellConfigs''' damagedCounts (length statusesM)
  guard $ unifiable statusesM wellStatuses
  pure wellStatuses

solveA :: ParsedStructure -> Int
solveA = sum . map (uncurry solveLineA)

------------
-- part b --
------------

wellConfigsB
  :: ([Status] -> Bool)
  -> [Int]
  -> Int
  -> [[Status]]
wellConfigsB _ _ total
  | total < 0 = []
wellConfigsB p damagedCounts total =
  case damagedCounts of
    [] -> do
      let res = replicate total Operational
      guard $ p res
      pure res
    [damaged] -> do
      pre_padding <- [0 .. total-damaged]
      let post_padding = total - damaged - pre_padding
      let res = replicate pre_padding Operational <>
                replicate damaged Damaged <>
                replicate post_padding Operational
      guard $ p res
      pure res
    damaged : drest -> concat
      [ [] -- warning go away
      , do
        rest <- wellConfigsB p damagedCounts (total - 1)
        let res = Operational : rest
        guard $ p res
        pure res
      , do
        rest <- wellConfigsB p drest (total - damaged - 1)
        let res = replicate damaged Damaged <> (Operational : rest)
        guard $ p res
        pure res
      ]

solveLineB :: Int -> [Maybe Status] -> [Int] -> Int
solveLineB n statusMs damagedCounts = length $
  wellConfigsB (unifiable newStatusMs) newDamagedCounts (length newStatusMs)
  where
    newStatusMs = intercalate [Nothing] (replicate n statusMs)
    newDamagedCounts = concat (replicate n damagedCounts)

solveB :: ParsedStructure -> Int
solveB lines =
  sum $ map (\(x, y, z, w) -> x) $ map traceShowId (zipWith3 f l1 l2 l3)
          --sum $ map (uncurry (solveLineB 5)) lines
  where
    --lines = test
    l1 = map (uncurry (solveLineB 1)) lines
    l2 = map (uncurry (solveLineB 2)) lines
    l3 = map (uncurry (solveLineB 3)) lines
    f x y z = (x, y, z, fromIntegral y / fromIntegral x == fromIntegral z / fromIntegral y)

test :: ParsedStructure
test = fromJust $ P.parseMaybe parser "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1\n"

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
