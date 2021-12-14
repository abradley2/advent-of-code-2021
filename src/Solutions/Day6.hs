{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Solutions.Day6 where

import           Data.Foldable      (toList)
import           Data.HashMap.Lazy  (HashMap, insert, keys, lookup)
import qualified Data.HashMap.Lazy  as HashMap
import           Data.Hashable
import           Data.List          ((!!))
import           Data.List.Split    (chunksOf)
import           Data.Sequence      (mapWithIndex)
import qualified Data.Sequence      as Seq (fromList)
import           Relude
import           Solutions.Evaluate (Label (..), Solution, evaluate)
import           Text.Parsec        (char, eof, many1, noneOf, try)
import qualified Text.Parsec        as P ((<|>))
import           Text.Parsec.Text   (Parser)
import           Text.Read          (readMaybe)

type Input = [Int]

type Cache = HashMap (Int, Int) [Int]

partOne :: Int -> Input -> Int
partOne dayCount input = length $ simulateDays dayCount input

elapsable :: Int -> Int -> Int
elapsable duration curVal
  | duration > curVal = curVal
  | otherwise = duration

partTwo :: Int -> Cache -> Int -> [Int] -> Text
partTwo count cache 0 input = show $ keys cache
-- partTwo cache 0 input = getSum . fold $ Sum . length <$> input
partTwo count cache dayCount input =
  let elapsed = elapsable 44 dayCount
      (nextCache, nextResults) =
        foldr
          (\fish (cache', fishes) ->
             let nextFishes =
                   fromMaybe
                     (simulateDays elapsed [fish])
                     (lookup (elapsed, fish) cache')
              in ( insert (elapsed, fish) nextFishes cache'
                 , nextFishes <> fishes))
          (cache, [])
          input
   in partTwo count nextCache (dayCount - elapsed) nextResults

simulateDays :: Int -> [Int] -> [Int]
simulateDays 0 input = input
simulateDays v input = simulateDays (v - 1) (simulateDay input)
  where
    simulateDay =
      foldr
        (\fish school ->
           case fish of
             (-1) -> 6 : 8 : school
             _    -> fish : school)
        [] .
      fmap (flip (-) 1)

partOneSampleSolution :: Solution
partOneSampleSolution =
  evaluate
    inputParser
    (partOne 18)
    (Label "Part One Sample")
    "src/Solutions/Day6/sample_input.txt"

partOneSolution :: Solution
partOneSolution =
  evaluate
    inputParser
    (partOne 80)
    (Label "Part One")
    "src/Solutions/Day6/sample_input.txt"

partTwoSampleSolution :: Solution
partTwoSampleSolution =
  evaluate
    inputParser
    (partTwo 0 mempty 80)
    (Label "Part Two Sample")
    "src/Solutions/Day6/sample_input.txt"

partTwoSolution :: Solution
partTwoSolution =
  evaluate
    inputParser
    (partTwo 0 mempty 264)
    (Label "Part Two")
    "src/Solutions/Day6/sample_input.txt"

inputParser :: Parser Input
inputParser = do
  x <- many1 (noneOf ",") >>= readInt
  xs <- try (eof >> pure []) P.<|> (char ',' >> inputParser)
  pure (x : xs)

readInt :: String -> Parser Int
readInt = maybe (fail "not an int") pure . readMaybe
