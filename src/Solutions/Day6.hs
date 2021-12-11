{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Solutions.Day6 where

import           Data.Foldable      (toList)
import           Data.HashMap.Lazy  (HashMap, insert, lookup)
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
partTwo dayCount input = simulateAll mempty $ (dayCount, ) <$> input

-- get a fishes liftime, a 6 to 0 descending cycle that repeats until "dayCount" reaches 0
type Cache = HashMap (Int, Int) [(Int, Int)]

flattenInputs :: Int -> [Int] -> [(Int, Int)]
flattenInputs days = join . fmap (flattenInput days)

flattenInput :: Int -> Int -> [(Int, Int)]
flattenInput days fish =
  let delimiter = 4
      -- for a day count of 13 and a delimiter of 4, we'll want "3 chunks of 4, and a remainder chunk of 1"
      (dayChunks, remainder) = days `divMod` delimiter
      -- convert the previous into a proper list "[4, 4, 4, 1]"
      dayVals = replicate dayChunks delimiter <> [remainder]
      -- to get the same concept for fish values, we need to get the fish lifetime,
      -- chunk it by the same delimiter, and take the fishes "starting value" at each point.
      -- a fish of value "3" at day 13 will have starting points at intervals of 4 of [3, 6, 2, 5]
      fishVals = fmap (!! 0) $ chunksOf delimiter $ fishLifetime days fish
     -- finally we combine the two by index
   in toList $
      mapWithIndex (\idx dayVal -> (dayVal, fishVals !! idx)) $
      Seq.fromList dayVals
  where
    fishLifetime :: Int -> Int -> [Int]
    fishLifetime 0 val         = []
    fishLifetime dayCount (-1) = 6 : fishLifetime (dayCount - 1) 5
    fishLifetime dayCount val  = val : fishLifetime (dayCount - 1) (val - 1)

simulateAll :: Cache -> [(Int, Int)] -> Int
simulateAll cache [] = 0
simulateAll cache ((days, fish):xs) =
  let
   in 1 + simulateAll cache (simulate (days, fish) <> xs)

simulate :: (Int, Int) -> [(Int, Int)]
simulate (-1, _)      = []
simulate (days, -1)   = (days, 6) : simulate (days - 1, 7)
simulate (days, fish) = simulate (days - 1, fish - 1)

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

partTwoSampleSolution :: Solution
partTwoSampleSolution =
  evaluate
    inputParser
    (partTwo 80)
    (Label "Part Two Sample")
    "src/Solutions/Day6/sample_input.txt"

partTwoSolution :: Solution
partTwoSolution =
  evaluate
    inputParser
    (partTwo 80)
    (Label "Part Two")
    "src/Solutions/Day6/input.txt"

inputParser :: Parser Input
inputParser = do
  x <- many1 (noneOf ",") >>= readInt
  xs <- try (eof >> pure []) P.<|> (char ',' >> inputParser)
  pure (x : xs)

readInt :: [Char] -> Parser Int
readInt = maybe (fail "not an int") pure . readMaybe
