{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

module Solutions.Day4 where

import Data.IntMap.Lazy as IntMap (IntMap, insert)
import Data.Foldable (toList)
import Relude
import Solutions.Evaluate (evaluate, Label(..), Solution)
import Text.Parsec
import qualified Text.Parsec as P ((<|>))
import Text.Parsec.Text (Parser)
import Text.Read (readMaybe)
import Data.Sequence (mapWithIndex)

type Entries = [Int]

type Table = IntMap (IntMap (Int, Bool))

partOneSampleSolution :: Solution
partOneSampleSolution = evaluate inputParser partOne (Label "Part One Sample") "src/Solutions/Day4/sample_input.txt"

partOne :: (Entries, [Table]) -> Text
partOne = show

tableParser :: Int -> Table -> Parser Table
tableParser rowIdx table = do
  row <- toIntMap . toList . mapWithIndex (,) . fromList <$> rowParser
  let nextTable = IntMap.insert rowIdx row table
  (newline >> tableParser (rowIdx + 1) nextTable) P.<|> pure nextTable
  where
    toIntMap :: [(Int, a)] -> IntMap a
    toIntMap = fromList

    rowParser :: Parser [(Int, Bool)]
    rowParser = do
      val <- (many1 (noneOf " \n") >>= parseInt "Row value is not an int") <&> (,False)
      next <- (many1 (char ' ') >> rowParser) P.<|> (newline >> mempty)
      pure (val:next)

inputParser :: Parser (Entries, [Table])
inputParser = do
  entries <- entriesParser
  newline >> newline
  tables <- tablesParser
  pure (entries, tables)
  where
    entriesParser :: Parser Entries
    entriesParser = do
      val <- many1 (noneOf ",\n") >>= parseInt "Entry is not an int"
      next <- (char ',' >> entriesParser) P.<|> pure []
      pure (val : next)
    tablesParser :: Parser [Table]
    tablesParser = do
      table <- tableParser 0 mempty
      next <- (newline >>= const tablesParser) P.<|> pure []
      pure (table:next)

parseInt :: [Char] -> [Char] -> ParsecT Text () Identity Int
parseInt err = maybe (fail err) pure . readMaybe