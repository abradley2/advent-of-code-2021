{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Solutions.Day4 where

import           Data.Foldable      (toList)
import           Data.IntMap.Lazy   as IntMap (IntMap, insert)
import           Data.Sequence      (mapWithIndex)
import qualified Data.Sequence      as Seq (fromList)
import           Data.Set.Ordered
import qualified Data.Set.Ordered   as Set (fromList)
import           Relude
import           Solutions.Evaluate (Label (..), Solution, evaluate)
import           Text.Parsec
import qualified Text.Parsec        as P ((<|>))
import           Text.Parsec.Text   (Parser)
import           Text.Read          (readMaybe)

type Entries = [Int]

type Table = IntMap (IntMap (Int, Bool))

partOneSampleSolution :: Solution
partOneSampleSolution =
  evaluate
    inputParser
    partOne
    (Label "Part One Sample")
    "src/Solutions/Day4/sample_input.txt"

partOne :: (Entries, [Table]) -> Text
partOne = show

a :: IntMap (Int, Bool) -> IntMap (Int, Bool)
a = fmap (\v -> v)

checkTable :: Table -> Maybe Entries
checkTable table = Nothing

scoreTable :: Entries -> Table -> Maybe Text
scoreTable (entry:next) table =
  let updated =
        fmap
          (\(colVal, isChecked) ->
             if colVal == entry
               then (colVal, True)
               else (colVal, isChecked)) <$>
        table
   in Nothing

{- PARSING -}
tableParser :: Int -> Table -> Parser Table
tableParser rowIdx table = do
  row <- toIntMap . toList . mapWithIndex (,) . Seq.fromList <$> rowParser
  let nextTable = IntMap.insert rowIdx row table
  try (newline >> noneOf "\n" >> tableParser (rowIdx + 1) nextTable) P.<|>
    pure nextTable
  where
    toIntMap :: [(Int, a)] -> IntMap a
    toIntMap = fromList
    rowParser :: Parser [(Int, Bool)]
    rowParser = do
      void (many1 (char ' ')) P.<|> pure ()
      val <-
        (many1 (noneOf " \n") >>= parseInt "Row value is not an int") <&>
        (, False)
      next <- (many1 (char ' ') >> rowParser) P.<|> mempty
      pure (val : next)

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
      next <- (newline >> newline >>= const tablesParser) P.<|> (eof >> pure [])
      pure (table : next)

parseInt :: [Char] -> [Char] -> ParsecT Text () Identity Int
parseInt err = maybe (fail err) pure . readMaybe
