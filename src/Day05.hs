{-# LANGUAGE OverloadedStrings #-}
module Day05 (parser, solutions) where

import Data.Monoid
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

-----------
-- types --
-----------

data RangeTransform = RangeTransform
  { transformStart :: Int
  , transformLength :: Int
  , shift :: Int
  }
  deriving (Eq, Ord, Show)

type RangeMaps = Set RangeTransform

data Almanac = Almanac
  { seeds                 :: [Int]
  , seedToSoil            :: RangeMaps
  , soilToFertilizer      :: RangeMaps
  , fertilizerToWater     :: RangeMaps
  , waterToLight          :: RangeMaps
  , lightToTemperature    :: RangeMaps
  , temperatureToHumidity :: RangeMaps
  , humidityToLocation    :: RangeMaps
  }
  deriving Show

-------------
-- parsing --
-------------

type Parser = P.Parsec Void Text

parseRangeTransform :: Parser RangeTransform
parseRangeTransform = do
  d <- L.decimal
  _ <- P.hspace
  s <- L.decimal
  _ <- P.hspace
  l <- L.decimal
  _ <- P.string ""  -- no hlint
  pure $ RangeTransform s l (d - s)

parseRangeMaps :: Parser RangeMaps
parseRangeMaps =
  S.fromList <$> parseRangeTransform `P.sepEndBy1` P.newline

parseAlmanac :: Parser Almanac
parseAlmanac = do
  _ <- P.string "seeds: "
  seeds <- L.decimal `P.sepBy` P.hspace
  _ <- P.newline >> P.newline
  _ <- P.string "seed-to-soil map:" >> P.newline
  map1 <- parseRangeMaps <* P.newline
  _ <- P.string "soil-to-fertilizer map:" >> P.newline
  map2 <- parseRangeMaps <* P.newline
  _ <- P.string "fertilizer-to-water map:" >> P.newline
  map3 <- parseRangeMaps <* P.newline
  _ <- P.string "water-to-light map:" >> P.newline
  map4 <- parseRangeMaps <* P.newline
  _ <- P.string "light-to-temperature map:" >> P.newline
  map5 <- parseRangeMaps <* P.newline
  _ <- P.string "temperature-to-humidity map:" >> P.newline
  map6 <- parseRangeMaps <* P.newline
  _ <- P.string "humidity-to-location map:" >> P.newline
  map7 <- parseRangeMaps
  _ <- P.string ""  -- no hlint
  pure $ Almanac seeds map1 map2 map3 map4 map5 map6 map7

------------
-- part a --
------------

applyRangeMapsA :: RangeMaps -> Int -> Int
applyRangeMapsA maps n =
  case RangeTransform (n+1) 0 0 `S.lookupLT` maps of
    Nothing -> n
    Just rt@(RangeTransform _ _ shift) ->
      if n `inRangeSource` rt then n + shift else n
  where
    k `inRangeSource` RangeTransform s l _ =
      s <= k && k < s + l


solveA :: Almanac -> Int
solveA (Almanac seeds map1 map2 map3 map4 map5 map6 map7) =
  minimum $ map location_from_soil seeds
  where
    location_from_soil =
      appEndo $
        mconcat $
          Endo . applyRangeMapsA <$>
            reverse [map1, map2, map3, map4, map5, map6, map7]

------------
-- part b --
------------

data Range = Range
  { start :: Int
  , length :: Int
  }
  deriving (Eq, Ord, Show)

type RangeSet = Set Range

-- precondition: xs <= ys
combineRanges :: Range -> Range -> Maybe Range
combineRanges (Range xs xl) (Range ys yl) =
  if ys <= xs + xl
  then Just $ Range xs (ys - xs + yl)
  else Nothing

normalizeRangeSet :: RangeSet -> RangeSet
normalizeRangeSet ranges =
  S.fromDistinctDescList $
    uncurry (:) $
      S.foldl go (firstRange, []) ranges
  where
    firstRange = S.findMin ranges

    go :: (Range, [Range]) -> Range -> (Range, [Range])
    go (acc, processed) r =
      case combineRanges acc r of
        Nothing -> (r , acc : processed)
        Just r' -> (r', processed)

rangesIntersect :: Range -> Range -> Maybe Range
rangesIntersect r1 r2 =
  case compare lower upper of
    LT -> Just $ Range lower (upper - lower)
    _  -> Nothing
  where
    Range s1 l1 = r1
    Range s2 l2 = r2
    lower = max s1 s2
    upper = min (s1 + l1) (s2 + l2)

-- returns Nothing if big does not intersect small
rangeMinus :: Range -> Range -> Maybe (Maybe Range, Range, Maybe Range)
rangeMinus big small = do
  Range s l <- rangesIntersect big small
  pure ( guardBool (s /= bs) $ Range bs (s - bs)
       , Range s l
       , guardBool (s + l /= bs + bl) $ Range (s + l) (bs + bl - s - l)
       )
  where
    guardBool b x = if b then Just x else Nothing
    Range bs bl = big

rangeShift :: Int -> Range -> Range
rangeShift n (Range s l) = Range (s + n) l

-- returns Nothing if nothing was mapped
applyRangeTransform
  :: RangeTransform
  -> Range
  -> Maybe (Maybe Range, Range, Maybe Range)
applyRangeTransform transform range = do
  (left, intersection, right) <- rangeMinus range (Range t_source t_length)
  pure (left, rangeShift shift intersection, right)
  where
    RangeTransform t_source t_length shift = transform

applyRangeMapsB' :: RangeMaps -> Range -> RangeSet
applyRangeMapsB' maps range =
  let (done, remaining_m) = S.foldl' go (S.empty, Just range) maps in
    case remaining_m of
      Nothing -> done
      Just remaining -> S.insert remaining done
  where
    go :: (RangeSet, Maybe Range)
       -> RangeTransform
       -> (RangeSet, Maybe Range)
    go (processed, Nothing) _ = (processed, Nothing)
    go (processed, Just unprocessed) transform =
      case applyRangeTransform transform unprocessed of
        Nothing -> (processed, Just unprocessed)
        Just (Nothing, output, after) ->
          (S.insert output processed, after)
        Just (Just before, output, after) ->
          (S.insert before $ S.insert output processed, after)

applyRangeMapsB :: RangeMaps -> RangeSet -> RangeSet
applyRangeMapsB maps ranges =
  normalizeRangeSet $
    S.unions $
      S.map (applyRangeMapsB' maps) ranges

solveB :: Almanac -> Int
solveB (Almanac seeds map1 map2 map3 map4 map5 map6 map7) =
  let seedRanges = S.fromList $ getSeedRanges seeds
      m1 = applyRangeMapsB map1
      m2 = applyRangeMapsB map2
      m3 = applyRangeMapsB map3
      m4 = applyRangeMapsB map4
      m5 = applyRangeMapsB map5
      m6 = applyRangeMapsB map6
      m7 = applyRangeMapsB map7
      finalRanges = m7 $ m6 $ m5 $ m4 $ m3 $ m2 $ m1 seedRanges
  in start $ S.findMin finalRanges
  where
    getSeedRanges [] = []
    getSeedRanges (a:b:rest) = Range a b : getSeedRanges rest
    getSeedRanges _ =
      error "Almanac should have an even number of seed entries"
    
--------------
-- exported --
--------------

parser :: Parser Almanac
parser = parseAlmanac

solutions :: [Almanac -> Text]
solutions = map (showOutput .) [solveA, solveB]
  where showOutput = T.pack . show
