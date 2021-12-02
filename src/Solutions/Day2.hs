{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Solutions.Day2 where

import           Relude
import           Solutions.Evaluate     (Label (..), Solution, evaluate)
import           Text.Parsec            as P (char, newline, noneOf, parse,
                                              (<|>))
import           Text.Parsec.Combinator
import           Text.Parsec.Text       (Parser)
import           Text.Read              (readMaybe)

data Position =
  Position
    { depth    :: Int
    , distance :: Int
    , aim      :: Int
    }

data Instruction
  = Forward Int
  | Upward Int
  | Downward Int

solvePartTwo :: [Instruction] -> Position -> Int
solvePartTwo (instruction:nextInstructions) pos =
  solvePartTwo nextInstructions $
  case instruction of
    Downward val -> pos {aim = aim pos + val}
    Upward val -> pos {aim = aim pos - val}
    Forward val ->
      pos {distance = distance pos + val, depth = depth pos + (aim pos * val)}
solvePartTwo [] pos = depth pos * distance pos

solvePartOne :: [Instruction] -> Position -> Int
solvePartOne (instruction:nextInstructions) pos =
  solvePartOne nextInstructions $
  case instruction of
    Forward val  -> pos {distance = distance pos + val}
    Upward val   -> pos {depth = depth pos - val}
    Downward val -> pos {depth = depth pos + val}
solvePartOne [] pos = depth pos * distance pos

partOne = evaluate inputParser (`solvePartOne` Position 0 0 0)

partOneSampleSolution :: Solution
partOneSampleSolution =
  partOne (Label "Part One Sample Input") "src/Solutions/Day2/sample_input.txt"

partOneSolution :: Solution
partOneSolution = partOne (Label "Part One") "src/Solutions/Day2/input.txt"

partTwo = evaluate inputParser (`solvePartTwo` Position 0 0 0)

partTwoSampleSolution :: Solution
partTwoSampleSolution =
  partTwo (Label "Part Two Sample Input") "src/Solutions/Day2/sample_input.txt"

partTwoSolution :: Solution
partTwoSolution = partTwo (Label "Part Two") "src/Solutions/Day2/input.txt"

inputParser :: Parser [Instruction]
inputParser = do
  direction <-
    many1 (noneOf "\n ") >>= \case
      "up"      -> pure Upward
      "down"    -> pure Downward
      "forward" -> pure Forward
  _ <- many1 (char ' ')
  speed <-
    many1 (noneOf "\n") >>=
    (\str ->
       case readInt str of
         Nothing  -> fail "Not an int"
         Just val -> pure val)
  let instruction = direction speed
  nextInstructions <- (newline >>= const inputParser) P.<|> pure []
  pure (instruction : nextInstructions)
  where
    readInt :: [Char] -> Maybe Int
    readInt = readMaybe
