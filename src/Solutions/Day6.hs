{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day6 where

import           Data.Foldable      (toList)
import           Data.HashMap.Lazy  (HashMap, insert, lookup)
import qualified Data.HashMap.Lazy  as HashMap
import           Data.Hashable
import           Data.Sequence      (mapWithIndex)
import qualified Data.Sequence      as Seq (fromList)
import           Relude
import           Solutions.Evaluate (Label (..), Solution, evaluate)
import           Text.Parsec        (char, eof, many1, noneOf, try)
import qualified Text.Parsec        as P ((<|>))
import           Text.Parsec.Text   (Parser)
import           Text.Read          (readMaybe)

-- Surely, each lanternfish creates a new lanternfish once every 7 days.
--
-- one lanternfish might have 2 days left until it creates another lanternfish, while another might have 4.
--
--  So, you can model each fish as a single number that represents the number of days until it creates a new lanternfish.
--
-- each day -1. On hitting negative one, spawn a new fish and set to 6
--
-- the new lanternfish spawns as an 8
type Input = [Int]

partOne :: Int -> Input -> Int
partOne dayCount input = length $ simulateDays dayCount input

partTwo :: Int -> [Int] -> Int
partTwo dayCount input = length $ simulateDaysChunked dayCount input mempty

data CacheEntry a =
  CacheEntry
    { initialValue  :: Int
    , elapsableDays :: Int
    }
  deriving (Eq, Generic, Show)

instance Hashable a => Hashable (CacheEntry a)

type CacheMap = HashMap (CacheEntry ()) [Int]

simulateDaysChunked :: Int -> [Int] -> CacheMap -> [Int]
simulateDaysChunked days
  | days > 0 =
    \input cache ->
      let (fishesResult, cacheResult) =
            foldr
              (\fish (fishes, cache') ->
                 let (nextFishes, nextCache) = simulate18 fish days cache'
                  in (fishes <> nextFishes, nextCache))
              ([], cache)
              input
       in simulateDaysChunked (days - min days 18) fishesResult cacheResult
  | otherwise = \input cache -> input

simulateAll :: [(Int, Int)] -> [Int]
simulateAll ((days, fish):rest) = []

simulateChunk :: (Int, Int) -> [(Int, Int)]
simulateChunk (days, fish) =
  let elapsableDays' = 256 + fish
      (result, _) =
        foldr
          (\daysElapsed (school, fishVal) ->
             case fishVal of
               (-1) -> ((days - daysElapsed, 8) : school, 6)
               _    -> (school, fishVal - 1))
          ([], fish)
          [1 .. elapsableDays']
   in (0, fish) : result

simulate18 :: Int -> Int -> CacheMap -> ([Int], CacheMap)
simulate18 fishValue days cache =
  let elapsableDays' = days - min days 18
      cacheKey = CacheEntry fishValue elapsableDays'
      result =
        fromMaybe
          (simulateDays elapsableDays' [fishValue])
          (lookup cacheKey cache)
   in (result, insert cacheKey result cache)

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

simulateDay :: [Int] -> [Int]
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
    (partOne 80)
    (Label "Part One Sample")
    "src/Solutions/Day6/sample_input.txt"

partOneSolution :: Solution
partOneSolution =
  evaluate
    inputParser
    (partOne 80)
    (Label "Part One")
    "src/Solutions/Day6/input.txt"

partTwoSolution :: Solution
partTwoSolution =
  evaluate
    inputParser
    (partTwo 18)
    (Label "Part Two")
    "src/Solutions/Day6/sample_input.txt"

inputParser :: Parser Input
inputParser = do
  x <- many1 (noneOf ",") >>= readInt
  xs <- try (eof >> pure []) P.<|> (char ',' >> inputParser)
  pure (x : xs)

readInt :: [Char] -> Parser Int
readInt = maybe (fail "not an int") pure . readMaybe
