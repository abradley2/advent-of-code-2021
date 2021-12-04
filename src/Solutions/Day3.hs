{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day3 where

import           Data.Binary        as Bin
import           Data.IntMap.Lazy   as IntMap
import           Data.List          (elemIndices)
import           Relude
import           Solutions.Evaluate (Label (..), Solution, evaluate,
                                     evaluateText)
import           Text.Parsec        hiding (State)
import qualified Text.Parsec        as P ((<|>))
import           Text.Parsec.Text   (Parser)
import           Text.Read          (readMaybe)

data Bit
  = One
  | Zero

instance Enum Bit where
  toEnum 1 = One
  toEnum 0 = Zero

-- we'll count 0 and 1's for easy comparison by starting at an
-- index of 0, adding 1 when we hit 1, and removing 1 when we hit 0
bitToCount :: Bit -> Int -> Int
bitToCount One val  = val + 1
bitToCount Zero val = val - 1

bitFromCount :: Int -> Bit
bitFromCount val =
  if val > 0
    then One
    else Zero

binToDec :: [Int] -> Integer
binToDec = sum . fmap (2 ^) . elemIndices 1 . reverse

partOne :: Text -> Text
partOne input =
  (\(parseResult, intMap) ->
     let binList = flattenDigits intMap
      in case parseResult of
           Right _  -> show $ binToDec binList * binToDec (flipDigits binList)
           Left err -> show err) $
  flip runState mempty $ runPT partOneParser () "" input

partOneSampleSolution :: Solution
partOneSampleSolution =
  evaluateText
    partOne
    (Label "Part One Sample")
    "src/Solutions/Day3/sample_input.txt"

partOneSolution :: Solution
partOneSolution =
  evaluateText partOne (Label "Part One") "src/Solutions/Day3/input.txt"

withBit :: Bit -> Column -> IntMap Int -> IntMap Int
withBit bit =
  IntMap.alter
    (\case
       Just val -> Just $ bitToCount bit val
       Nothing  -> Just $ bitToCount bit 0)

flattenDigits :: IntMap Int -> [Int]
flattenDigits = fmap (fromEnum . bitFromCount . snd) . IntMap.assocs

flipDigits :: [Int] -> [Int]
flipDigits = fmap (abs . (\v -> v - 1))

partOneParser :: ParsecT Text () (State (IntMap Int)) ()
partOneParser = do
  bit <- alphaNum >>= (maybe (fail "not a digit") pure . readDigit)
  col <- sourceColumn <$> getPosition
  lift $ state $ \s -> ((), withBit bit col s)
  try (char '\n' >>= const partOneParser) P.<|> try partOneParser P.<|> eof
  pure ()
  where
    readDigit :: Char -> Maybe Bit
    readDigit c = toEnum <$> readMaybe [c]
