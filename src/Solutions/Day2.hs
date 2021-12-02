{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Solutions.Day2 where

import           Relude
import           Solutions.Evaluate     (evaluate)
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

evaluateSampleInput :: IO (Text, Text)
evaluateSampleInput =
  (, "Part One Sample") <$>
  evaluate
    inputParser
    "src/Solutions/Day2/sample_input.txt"
    (\ins -> solvePartOne ins (Position 0 0 0))

evaluatePartOne :: IO (Text, Text)
evaluatePartOne =
  (, "Part One") <$>
  evaluate
    inputParser
    "src/Solutions/Day2/input.txt"
    (\ins -> solvePartOne ins (Position 0 0 0))

evaluatePartTwoSample :: IO (Text, Text)
evaluatePartTwoSample =
  (, "Part Two Sample") <$>
  evaluate
    inputParser
    "src/Solutions/Day2/sample_input.txt"
    (\ins -> solvePartTwo ins (Position 0 0 0))

evaluatePartTwo :: IO (Text, Text)
evaluatePartTwo =
  (, "Part Two") <$>
  evaluate
    inputParser
    "src/Solutions/Day2/input.txt"
    (\ins -> solvePartTwo ins (Position 0 0 0))

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
