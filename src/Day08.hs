{-# LANGUAGE OverloadedStrings #-}
module Day08 (parser, solutions) where

import Control.Applicative
import Control.Exception (assert)
import Data.Functor
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Map.NonEmpty qualified as NEM
import Data.Set qualified as S
import Data.Set.NonEmpty qualified as NES
import GHC.Generics (Generic)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P


-----------
-- types --
-----------

type Node = Text

data Step = LStep | RStep
  deriving (Eq, Generic, Show)

data Document = Document (NonEmpty Step) (NonEmpty (Node, (Node, Node)))
  deriving Show

-------------
-- parsing --
-------------

type Parser = P.Parsec Text Text

parseStep :: Parser Step
parseStep = P.string "L" $> LStep
        <|> P.string "R" $> RStep

parsePaths :: Parser (Node, (Node, Node))
parsePaths = do
  source <- T.pack <$> many P.alphaNumChar
  _ <- P.string " = ("
  left <- T.pack <$> many P.alphaNumChar
  _ <- P.string ", "
  right <- T.pack <$> many P.alphaNumChar
  _ <- P.string ")"
  pure (source, (left, right))

parseDocument :: Parser Document
parseDocument = do
  steps <- NE.fromList <$> many parseStep
  _ <- P.newline >> P.newline
  directions <- NE.fromList <$> parsePaths `P.sepEndBy1` P.newline
  pure $ Document steps directions

type ParsedStructure = Document

------------
-- part a --
------------

solveA :: ParsedStructure -> Int
solveA (Document steps paths) = go 0 (NE.cycle steps) "AAA"
  where
    instructions = NEM.fromList paths

    dir LStep = fst
    dir RStep = snd

    go acc _ "ZZZ" = acc
    go acc (step :| rest) current =
      let next = dir step $ instructions NEM.! current
      in go (succ acc) (NE.fromList rest) next

------------
-- part b --
------------

solveB :: ParsedStructure -> Int
solveB (Document steps paths) =
  assert (and checks) $
    foldl1' lcm $ map (length . snd) cycles
  where
    edges = NEM.fromList paths
    allNodes = NES.fromList $ NE.map fst paths
    startNodes = NES.filter (\s -> T.last s == 'A') allNodes
    endNodes = NES.filter (\s -> T.last s == 'Z') allNodes

    steps_e = NE.zip (NE.fromList @Int [0..]) steps
    dir (_, step) = case step of { LStep -> fst; RStep -> snd }

    apStep step_e cur = dir step_e (edges NEM.! cur)

    cycleOfNode :: Text -> ([Text], [Text])
    cycleOfNode start =
      getCycle $ until done step begin
      where
        begin = ([], NE.cycle steps_e, start)
        done (history, (i, _) :| _, current) = (i, current) `elem` history
        step (history, step_e@(i, _) :| steps_rest, current) =
          ((i, current) : history, NE.fromList steps_rest, apStep step_e current)
        getCycle (history, (i, _) :| _, current) =
          let (header, repeater) = span (/= (i, current)) (reverse history) in
          (map snd header, map snd repeater)

    boolCycleOfNode start =
      let (header, repeater) = cycleOfNode start in
      let isEndNode = (`S.member` endNodes) in
      (map isEndNode header, map isEndNode repeater)

    cycles = map boolCycleOfNode $ S.toList startNodes
    boolIndices = map (findIndices id . snd) cycles

    checks = zip cycles boolIndices <&> \((header, repeater), endIndices) ->
      case endIndices of
        [endIndex] -> length repeater - length header == endIndex
        _ -> False

--------------
-- exported --
--------------

parser :: Parser ParsedStructure
parser = parseDocument

solutions :: [ParsedStructure -> Text]
solutions = (showOutput .) <$> [solveA, solveB]
  where
    showOutput :: Show a => a -> Text
    showOutput = T.pack . show
