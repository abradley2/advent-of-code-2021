{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module Solutions.Day10 where

import           Data.Foldable
import           Data.HashMap.Lazy  (HashMap, alter)
import qualified Data.HashMap.Lazy  as HashMap (toList)
import           Data.Set           (Set, difference)
import qualified Data.Set           as Set (fromList, singleton)
import           Data.Text          (pack)
import           Relude             hiding (many)
import           Solutions.Evaluate (Label (..), Solution, evaluateText)
import           Text.Parsec        (ParseError (..), ParsecT, char, eof,
                                     getPosition, many, many1, newline, noneOf,
                                     oneOf, runPT, try)
import qualified Text.Parsec        as P ((<|>))
import           Text.Parsec.Text   (Parser)

type Results = HashMap Char Int

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
foldParsers = foldr (\a b -> b P.<|> a) (do fail "")

addResult :: Char -> ParsecT Text () (State Results) ()
addResult c = state (\s -> ((), alter (Just . maybe 1 (+ 1)) c s))

discardRemainingLine :: Monad m => ParsecT Text () m ()
discardRemainingLine = void (many1 $ noneOf "\n")

corruptLineParser :: ParsecT Text () (State Results) ()
corruptLineParser = void $ foldParsers (mkParser <$> toList openers)
  where
    allParsers :: ParsecT Text () (State Results) ()
    allParsers = foldParsers $ mkParser <$> toList openers
    mkParser :: Char -> ParsecT Text () (State Results) ()
    mkParser c = do
      char c
      eof P.<|> void (many allParsers)
      eof P.<|> void (char (closerFor c)) P.<|>
        (do c <- noneOf "\n"
            _ <- addResult c
            discardRemainingLine) P.<|>
        pure ()

inputParser :: ParsecT Text () (State Results) ()
inputParser =
  void $ do
    _ <- corruptLineParser
    _ <- (try newline >> inputParser) P.<|> eof
    pure ()

partOne :: Text -> Text
partOne input =
  show $
  scoreResults $ snd $ flip runState mempty $ runPT inputParser () "Input" input

partOneSampleSolution :: Solution
partOneSampleSolution =
  evaluateText
    partOne
    (Label "Part One Sample")
    "src/Solutions/Day10/sample_input.txt"

partOneSolution :: Solution
partOneSolution =
  evaluateText partOne (Label "Part One") "src/Solutions/Day10/input.txt"
