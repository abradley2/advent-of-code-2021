{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Solutions.Day4 where

import           Data.Foldable      (toList)
import           Data.IntMap.Lazy   (IntMap)
import qualified Data.IntMap.Lazy   as IntMap (assocs, elems, fromList, insert)
import           Data.List          ((!!))
import           Data.Sequence      (mapWithIndex)
import qualified Data.Sequence      as Seq (fromList)
import           Data.Set           (Set, isSubsetOf)
import qualified Data.Set           as Set (fromList)
import           Data.Set.Ordered   (OSet)
import qualified Data.Set.Ordered   as OSet (elemAt, fromList, toSet)
import           Relude
import           Solutions.Evaluate (Label (..), Solution, evaluate)
import           Text.Parsec        hiding ((<|>))
import qualified Text.Parsec        as P ((<|>))
import           Text.Parsec.Text   (Parser)
import           Text.Read          (readMaybe)

type Entries = [Int]

type Table = IntMap (IntMap Int)

type TableSets = [OSet Int]

tableToSets :: Table -> TableSets
tableToSets table =
  let rowSets =
        foldr
          (\(rowIdx, columnMap) sets ->
             OSet.fromList (IntMap.elems columnMap) : sets)
          []
          (IntMap.assocs table)
      colSets =
        fmap (fmap OSet.fromList . toList) $
        sequence $
        mapWithIndex
          (\colIdx _ -> sequence $ (`OSet.elemAt` colIdx) <$> rowSets)
          (Seq.fromList rowSets)
   in rowSets <> fromMaybe [] colSets

partOneSampleSolution :: Solution
partOneSampleSolution =
  evaluate
    inputParser
    partOne
    (Label "Part One Sample")
    "src/Solutions/Day4/sample_input.txt"

checkTableSets :: Entries -> TableSets -> Maybe (Set Int, [Int])
checkTableSets entries [] = Nothing
checkTableSets entries (curSet:next) =
  checkTableSet (OSet.toSet curSet) [] entries <|> checkTableSets entries next

checkTableSet :: Set Int -> Entries -> Entries -> Maybe (Set Int, [Int])
checkTableSet set curEntries nextEntries =
  if set `isSubsetOf` Set.fromList curEntries
    then Just (set, curEntries)
    else case nextEntries of
           []           -> Nothing
           (entry:next) -> checkTableSet set (entry : curEntries) next

partOne :: (Entries, [Table]) -> Text
partOne (entries, tables') =
  let tables = tableToSets <$> tables'
      grades = catMaybes $ checkTableSets entries <$> tables
      winner =
        foldr
          (\cur champion ->
             if (length $ snd cur) < (fromMaybe 0 $ (length . snd) <$> champion)
               then Just cur
               else Just $ fromMaybe cur champion)
          (grades !!? 0)
          grades
   in show winner

checkTable :: Table -> Maybe Entries
checkTable table = Nothing

{- PARSING -}
tableParser :: Int -> Table -> Parser Table
tableParser rowIdx table = do
  row <-
    IntMap.fromList . toList . mapWithIndex (,) . Seq.fromList <$> rowParser
  let nextTable = IntMap.insert rowIdx row table
  try (newline >> noneOf "\n" >> tableParser (rowIdx + 1) nextTable) P.<|>
    pure nextTable
  where
    rowParser :: Parser [Int]
    rowParser = do
      void (many1 (char ' ')) P.<|> pure ()
      val <- many1 (noneOf " \n") >>= parseInt "Row value is not an int"
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
