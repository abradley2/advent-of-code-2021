{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Solutions.Day5 where

import           Data.HashMap.Lazy  (HashMap)
import qualified Data.HashMap.Lazy  as HashMap (alter, elems)
import           Data.Set           (Set)
import qualified Data.Set           as Set (fromList, size)
import           Relude
import           Relude.Extra
import           Solutions.Evaluate (Label (..), evaluate)
import           Text.Parsec        (char, many, many1, newline, noneOf, oneOf)
import qualified Text.Parsec        as P ((<|>))
import           Text.Parsec.Text   (Parser)
import           Text.Read          (readMaybe)

type Coord = (Int, Int)

type Path = (Coord, Coord)

type Input = [Path]

filterPaths :: [Path] -> [Path]
filterPaths = filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2)

explodePath :: (Coord, Coord) -> [Coord]
explodePath (cur, dest)
  | x1 < x2 = next (x1 + 1, y1)
  | x1 > x2 = next (x1 - 1, y1)
  | y1 < y2 = next (x1, y1 + 1)
  | y1 > y2 = next (x1, y1 - 1)
  | otherwise = []
  where
    (x1, y1) = cur
    (x2, y2) = dest
    next coords = coords : explodePath (coords, dest)

explodePathDiag :: (Coord, Coord) -> [Coord]
explodePathDiag (cur, dest)
  | cur /= dest = next (moveCoord cur dest)
  | otherwise = []
  where
    next coords = coords : explodePath (coords, dest)

moveCoord :: Coord -> Coord -> Coord
moveCoord (x1, y1) (x2, y2) = (move x1 x2, move y1 y2)
  where
    move :: Int -> Int -> Int
    move from to
      | from == to = to
      | from < to = from + 1
      | from > to = from - 1

countIntersections' :: [Coord] -> HashMap Coord Int
countIntersections' = foldr (HashMap.alter (Just . maybe 1 (+ 1))) mempty

sumIntersections :: HashMap Coord Int -> Int
sumIntersections = length . filter (\v -> v > 1) . HashMap.elems

partOne :: Input -> Text
partOne paths =
  show $
  sumIntersections $
  countIntersections' $
  join $ (\(cur, dest) -> cur : explodePath (cur, dest)) <$> filterPaths paths

partTwo :: Input -> Text
partTwo paths =
  show $
  sumIntersections $
  countIntersections' $
  join $ (\(cur, dest) -> cur : explodePathDiag (cur, dest)) <$> paths

partOneSampleSolution =
  evaluate
    inputParser
    partOne
    (Label "Part One Sample")
    "src/Solutions/Day5/sample_input.txt"

partOneSolution =
  evaluate inputParser partOne (Label "Part One") "src/Solutions/Day5/input.txt"

partTwoSampleSolution =
  evaluate
    inputParser
    partTwo
    (Label "Part Two Sample")
    "src/Solutions/Day5/sample_input.txt"

partTwoSolution =
  evaluate inputParser partTwo (Label "Part Two") "src/Solutions/Day5/input.txt"

coordParser :: Parser Coord
coordParser = do
  x <- many1 (noneOf ",") >>= parseInt
  void $ char ','
  y <- many1 (noneOf " -\n") >>= parseInt
  pure (x, y)

pathParser :: Parser Path
pathParser = do
  foo <- coordParser
  void $ char ' ' >> char '-' >> char '>' >> char ' '
  bar <- coordParser
  pure (foo, bar)

inputParser :: Parser Input
inputParser = do
  x <- pathParser
  xs <- (newline >> inputParser) P.<|> pure []
  pure $ x : xs

parseInt :: String -> Parser Int
parseInt = maybe (fail "Not an int") pure . readMaybe
