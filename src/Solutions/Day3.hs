{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day3 where

import Data.IntMap.Lazy as IntMap ( IntMap, alter, assocs )
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
  deriving (Show, Eq)

flipBit :: Bit -> Bit
flipBit One = Zero
flipBit Zero = One

instance Enum Bit where
  toEnum 1 = One
  toEnum 0 = Zero
  fromEnum One = 1
  fromEnum Zero = 0

bitToCount :: Bit -> Int -> Int
bitToCount One val  = val + 1
bitToCount Zero val = val - 1

bitFromCount :: Int -> Bit
bitFromCount val =
  if val > 0
    then One
    else Zero

maybeBitFromCount :: Int -> Maybe Bit
maybeBitFromCount val
  | val > 0 = Just One
  | val < 0 = Just Zero
  | otherwise = Nothing

binToDec :: [Int] -> Int
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

partTwoSampleSolution :: Solution
partTwoSampleSolution =
    evaluateText partTwo (Label "Part Two Sample") "src/Solutions/Day3/sample_input.txt"

partTwoSolution :: Solution
partTwoSolution =
  evaluateText partTwo (Label "Part Two") "src/Solutions/Day3/input.txt"

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

partTwoParser :: Parser [[Bit]]
partTwoParser = do
  x <- many1 (noneOf "\n" >>= maybe (fail "not a digit") pure . readDigit)
  xs <- (char '\n' >>= const partTwoParser) P.<|> pure []
  pure (x:xs)
  where
    readDigit :: Char -> Maybe Bit
    readDigit c = toEnum <$> readMaybe [c]


partTwo :: Text -> Text
partTwo rawInput =
  let
    counts = flip runState mempty $ runPT partOneParser () "" rawInput
    bytes = parse partTwoParser "" rawInput
  in
    case (bytes, counts) of
      (Right input, (Right _, counts')) -> show $ do
          oxygen <- getOxygen input 0
          co2 <- getCO2 input 0
          pure $ oxygen * co2
      (Left err, _) -> "Error parsing lines into bytes: " <> show err
      (_, (Left err, _)) -> "Error Parsing input into counts: " <> show err

getOxygen = reduceBytes (fromMaybe One) (==)

getCO2 = reduceBytes (fromMaybe Zero) (/=)

reduceBytes :: (Maybe Bit -> Bit) -> (Bit -> Bit -> Bool) -> [[Bit]] -> Int -> Either Text Int
reduceBytes defaultTo predicate [byte] _  = Right $ binToDec $ fromEnum <$> byte
reduceBytes defaultTo predicate [] _ = Left "Failed to find any matching byte"
reduceBytes defaultTo predicate bytes col =
  case fmap (fromMaybe One . maybeBitFromCount) -- convert to dominant bit, defaulting as appropriate
        $ fmap (foldr bitToCount 0) -- add them up
        $ sequence 
        $ (!!? col) <$> bytes -- first bit from each byte
      of
      Just val -> reduceBytes defaultTo predicate (filter (\byte -> fromMaybe False $ predicate val <$> byte !!? col) bytes) (col + 1)
      Nothing -> Left $ "Failed to get dominant bit at col " <> show col
