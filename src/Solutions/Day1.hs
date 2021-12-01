{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Solutions.Day1 where

import           Relude
import           Solutions.Evaluate (evaluate)
import           Text.Parsec
import qualified Text.Parsec        as P ((<|>))
import           Text.Parsec.Text   (Parser)
import           Text.Read          (readMaybe)

solvePartOne :: Ord a => [a] -> Int
solvePartOne (cur:(next:others)) =
  solvePartOne (next : others) +
  if next > cur
    then 1
    else 0
solvePartOne _ = 0

solvePartTwo ::
     Num a
  => Ord a =>
       [a] -> Int
solvePartTwo (a:b:c:d:rest) =
  solvePartTwo (b : c : d : rest) +
  if a + b + c < b + c + d
    then 1
    else 0
solvePartTwo _ = 0

evaluatePartOne :: IO (Text, Text)
evaluatePartOne =
  (, "Part One") <$>
  evaluate parseInput "src/Solutions/Day1/input.txt" solvePartOne

evaluatePartTwo :: IO (Text, Text)
evaluatePartTwo =
  (, "Part Two") <$>
  evaluate parseInput "src/Solutions/Day1/input.txt" solvePartTwo

parseInput :: Parser [Int]
parseInput = do
  x <-
    do chars <- many1 $ noneOf "\n"
       maybe (fail "Not an int") pure (readInt chars)
  xs <- (char '\n' >>= const parseInput) P.<|> pure []
  pure (x : xs)
  where
    readInt :: [Char] -> Maybe Int
    readInt = readMaybe
