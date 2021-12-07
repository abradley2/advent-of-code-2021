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
import           Data.Set           (Set, difference, isSubsetOf, union)
import qualified Data.Set           as Set (fromList)
import           Data.Set.Ordered   (OSet)
import qualified Data.Set.Ordered   as OSet (elemAt, fromList, toSet)
import           Relude
import           Solutions.Evaluate (Label (..), Solution, evaluate)
import           Text.Parsec        (ParsecT, char, eof, many1, newline, noneOf,
                                     oneOf, try)
import qualified Text.Parsec        as P ((<|>))
import           Text.Parsec.Text   (Parser)
import           Text.Read          (readMaybe)

type Entries = [Int]

type Table = [OSet Int]

-- 43010
-- 22962
partOneSampleSolution :: Solution
partOneSampleSolution =
  evaluate
    inputParser
    partOne
    (Label "Part One Sample")
    "src/Solutions/Day4/sample_input.txt"

partOneSolution :: Solution
partOneSolution =
  evaluate inputParser partOne (Label "Part One") "src/Solutions/Day4/input.txt"

checkTable :: Entries -> Entries -> Table -> Maybe (Set Int, [Int])
checkTable entries nextEntries table =
  let match =
        foldl'
          (\acc tableRowCol ->
             acc <|> checkTableRowCol (OSet.toSet tableRowCol) entries)
          Nothing
          table
   in case match of
        Just m -> Just m
        Nothing ->
          case nextEntries of
            (next:nextEntries) -> checkTable (next : entries) nextEntries table
            []                 -> Nothing

checkTableRowCol :: Set Int -> Entries -> Maybe (Set Int, [Int])
checkTableRowCol set entries =
  if set `isSubsetOf` Set.fromList entries
    then Just (set, entries)
    else Nothing

scoreTable :: Entries -> Table -> (Int, (Int, Int), [Int])
scoreTable (calledEntry:nextEntries) table =
  let entries' = Set.fromList (calledEntry : nextEntries)
      table' = Set.fromList $ table >>= toList
      unmarkedValue = getSum $ foldMap Sum . toList $ difference table' entries'
   in ( calledEntry * unmarkedValue
      , (calledEntry, unmarkedValue)
      , calledEntry : nextEntries)

partOne :: (Entries, [Table]) -> Text
partOne (firstEntry:nextEntries, tables) =
  let grades =
        catMaybes $
        (\table -> (table, ) <$> checkTable [firstEntry] nextEntries table) .
        appendColumnSets <$>
        tables
      winner =
        foldr
          (\cur champion ->
             let (table, (_, calledEntries)) = cur
                 (_, (_, calledEntries')) = champion
              in if length calledEntries > length calledEntries'
                   then cur
                   else champion)
          (grades !! 0)
          grades
   in show $
      (\(table, (_, calledEntries)) -> scoreTable calledEntries table) winner

appendColumnSets :: Table -> Table
appendColumnSets tableRows' =
  let tableColumns =
        toList $
        mapWithIndex
          (\colIdx _ -> (!! colIdx) . toList <$> tableRows')
          (Seq.fromList tableRows')
   in tableRows' <> (OSet.fromList <$> tableColumns)

{- PARSING -}
tableParser :: Parser Table
tableParser = do
  row <- OSet.fromList <$> rowParser
  nextRows <- try (newline >> tableParser) P.<|> pure []
  let tableRows = row : nextRows
  pure tableRows
  where
    rowParser :: Parser [Int]
    rowParser = do
      void $ many $ char ' '
      val <- many1 (noneOf " \n") >>= parseInt "Row value is not an int"
      next <- (many1 (oneOf " ") >> rowParser) P.<|> mempty
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
      table <- tableParser
      next <- (newline >> newline >>= const tablesParser) P.<|> (eof >> pure [])
      pure (table : next)

parseInt :: String -> String -> ParsecT Text () Identity Int
parseInt err = maybe (fail err) pure . readMaybe

whitespace :: Parser ()
whitespace = void $ many1 $ oneOf " \t"

ignoreWhitespace :: Parser ()
ignoreWhitespace = void $ many $ oneOf " \t"
