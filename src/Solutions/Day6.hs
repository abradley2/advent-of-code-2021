{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Solutions.Day6 where

import           Data.Foldable      (toList)
import           Data.HashMap.Lazy  (HashMap, alter, elems, insert, keys,
                                     lookup)
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

data Group =
  Group
    { dayCount'  :: Int
    , fishVal'   :: Int
    , fishCount' :: Int
    }
  deriving (Show)

resolveGroups :: [(Int, Int)] -> [Group]
resolveGroups = join . map resolveGroup . toGroups

resolveGroup :: Group -> [Group]
resolveGroup g =
  let dayCount = dayCount' g
      fishVal = fishVal' g
      fishCount = fishCount' g
      (spawns, _) = (dayCount - fishVal) `quotRem` 7
      next = (\v -> Group (dayCount - (7 * v)) 6 fishCount) <$> [1 .. spawns]
   in g : (next >>= resolveGroup)

toGroups :: [(Int, Int)] -> [Group]
toGroups vals =
  (\((dayCount, fishVal), fishCount) -> Group dayCount fishVal fishCount) <$>
  HashMap.toList (groupCounts vals)
  where
    groupCounts :: [(Int, Int)] -> HashMap (Int, Int) Int
    groupCounts [] = mempty
    groupCounts ((dayCount, value):xs) =
      alter (Just . maybe 1 (+ 1)) (dayCount, value) (groupCounts xs)

partOne :: Int -> Input -> Int
partOne dayCount input = length $ simulateDays dayCount input

partTwo :: Int -> [Int] -> Text
partTwo days input =
  show . getSum . fold $
  map (\v -> Sum $ fishCount' v) . resolveGroups $ (days, ) <$> input

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
    (partTwo 80)
    (Label "Part Two Sample")
    "src/Solutions/Day6/sample_input.txt"

partTwoSolution :: Solution
partTwoSolution =
  evaluate
    inputParser
    (partTwo 264)
    (Label "Part Two")
    "src/Solutions/Day6/sample_input.txt"

inputParser :: Parser Input
inputParser = do
  x <- many1 (noneOf ",") >>= readInt
  xs <- try (eof >> pure []) P.<|> (char ',' >> inputParser)
  pure (x : xs)

readInt :: String -> Parser Int
readInt = maybe (fail "not an int") pure . readMaybe
