{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module Solutions.Day10 where

import           Data.Foldable
import           Data.HashMap.Lazy  (HashMap, alter, elems)
import qualified Data.HashMap.Lazy  as HashMap (toList)
import           Data.List          (delete)
import           Data.Text          (pack)
import           Relude             hiding (many, (<|>))
import           Solutions.Evaluate (Label (..), Solution, evaluateText)
import           Text.Parsec        (ParseError (..), ParsecT, char, eof,
                                     getPosition, many, many1, newline, noneOf,
                                     oneOf, runPT, try, (<|>))
import           Text.Parsec.Pos    (sourceLine)
import           Text.Parsec.Text   (Parser)

type Results = HashMap Char Int

addResult :: Char -> ParsecT Text () (State Results) ()
addResult c = state (\s -> ((), alter (Just . maybe 1 (+ 1)) c s))

type CloserResults = HashMap Int String

addCloserResult :: Int -> Char -> ParsecT Text () (State CloserResults) ()
addCloserResult l c = state (\s -> ((), alter (Just . maybe [c] (<> [c])) l s))

popCloserResult :: Int -> Char -> ParsecT Text () (State CloserResults) ()
popCloserResult l c =
  state (\s -> ((), alter (Just . maybe "ERROR" (removeLast c)) l s))

removeLast :: Char -> [Char] -> [Char]
removeLast c = reverse . delete c . reverse

scoreResults :: Results -> Int
scoreResults =
  foldr
    (\(char, x) b ->
       case char of
         ')' -> b + (3 * x)
         ']' -> b + (57 * x)
         '}' -> b + (1197 * x)
         '>' -> b + (25137 * x))
    0 .
  HashMap.toList

openers :: [Char]
openers = ['(', '[', '<', '{']

closerFor :: Char -> Char
closerFor '(' = ')'
closerFor '[' = ']'
closerFor '<' = '>'
closerFor '{' = '}'

foldParsers :: forall s u (m :: * -> *) a. [ParsecT s u m a] -> ParsecT s u m a
foldParsers = foldr (\a b -> b <|> a) (do fail "")

discardRemainingLine :: Monad m => ParsecT Text () m ()
discardRemainingLine = void (many1 $ noneOf "\n")

incompleteLineParser :: ParsecT Text () (State CloserResults) ()
incompleteLineParser = void $ foldParsers (mkParser <$> toList openers)
  where
    allParsers :: ParsecT Text () (State CloserResults) ()
    allParsers = foldParsers $ mkParser <$> toList openers
    mkParser :: Char -> ParsecT Text () (State CloserResults) ()
    mkParser c = do
      let closer = closerFor c
      line <- sourceLine <$> getPosition
      char c >> addCloserResult line closer
      many allParsers
      try (char closer >> popCloserResult line closer) <|> void newline <|> eof

corruptLineParser :: ParsecT Text () (State Results) ()
corruptLineParser = void $ foldParsers (mkParser <$> toList openers)
  where
    allParsers :: ParsecT Text () (State Results) ()
    allParsers = foldParsers $ mkParser <$> toList openers
    mkParser :: Char -> ParsecT Text () (State Results) ()
    mkParser c = do
      char c
      eof <|> void (many1 allParsers)
      eof <|> void (char (closerFor c)) <|>
        (do c <- noneOf "\n"
            _ <- addResult c
            discardRemainingLine) <|>
        pure ()

inputParser :: ParsecT Text () (State s) () -> ParsecT Text () (State s) ()
inputParser lineParser =
  void $ do
    _ <- lineParser
    _ <- eof <|> inputParser lineParser
    pure ()

partOne :: Text -> Text
partOne input =
  show $
  scoreResults $
  snd $
  flip runState mempty $ runPT (inputParser corruptLineParser) () "Input" input

partTwo :: Text -> Text
partTwo input =
  show $
  flip runState mempty $ runPT (many incompleteLineParser) () "Input" input

partOneSampleSolution :: Solution
partOneSampleSolution =
  evaluateText
    partOne
    (Label "Part One Sample")
    "src/Solutions/Day10/sample_input.txt"

partOneSolution :: Solution
partOneSolution =
  evaluateText partOne (Label "Part One") "src/Solutions/Day10/input.txt"

partTwoSampleSolution :: Solution
partTwoSampleSolution =
  evaluateText
    partTwo
    (Label "Part Two Sample")
    "src/Solutions/Day10/sample_input.txt"
