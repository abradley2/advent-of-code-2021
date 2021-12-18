{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module Solutions.Day10 where

import           Data.Foldable
import           Data.HashMap.Lazy  (HashMap, alter, elems)
import qualified Data.HashMap.Lazy  as HashMap (delete, toList)
import           Data.List          (delete, (!!))
import           Data.Text          (pack)
import           Relude             hiding (many, (<|>))
import           Solutions.Evaluate (Label (..), Solution, evaluateText)
import           Text.Parsec        (ParseError (..), ParsecT, char, eof,
                                     getPosition, many, many1, newline, noneOf,
                                     oneOf, parse, runPT, try, (<|>))
import           Text.Parsec.Text   (Parser)

type Results = HashMap Char Int

addResult :: Char -> ParsecT Text () (State Results) ()
addResult c = state (\s -> ((), alter (Just . maybe 1 (+ 1)) c s))

addCloserResult :: Char -> ParsecT Text () (State String) ()
addCloserResult c = state (\s -> ((), s <> [c]))

popCloserResult :: Char -> ParsecT Text () (State String) ()
popCloserResult c = state (\s -> ((), removeLast c s))

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

scorePartTwo :: [Char] -> Int
scorePartTwo =
  foldl'
    (\b a ->
       (b * 5) +
       (case a of
          ')' -> 1
          ']' -> 2
          '}' -> 3
          '>' -> 4))
    0

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

incompleteLineParser :: ParsecT Text () (State String) ()
incompleteLineParser = void $ foldParsers (mkParser <$> toList openers)
  where
    allParsers :: ParsecT Text () (State String) ()
    allParsers = foldParsers $ mkParser <$> toList openers
    mkParser :: Char -> ParsecT Text () (State String) ()
    mkParser c = do
      let closer = closerFor c
      char c >> addCloserResult closer
      many allParsers
      try
        ((char closer >> popCloserResult closer) >> (try allParsers <|> pure ())) <|>
        eof

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
  let lines =
        getMiddle .
        map (scorePartTwo . reverse . snd) .
        filter (\(res, _) -> isRight res) .
        map
          ((flip runState mempty . runPT incompleteLineParser () "Input") . pack) <$>
        parse parser "Input" input
   in show lines
  where
    parser = do
      x <- many1 $ noneOf "\n"
      xs <- (char '\n' >> parser) <|> (eof >> pure [])
      pure (x : xs)

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

partTwoSolution :: Solution
partTwoSolution =
  evaluateText partTwo (Label "Part Two") "src/Solutions/Day10/input.txt"

getMiddle :: Ord a => [a] -> a
getMiddle l = sort l !! fst (quotRem (length l) 2)
