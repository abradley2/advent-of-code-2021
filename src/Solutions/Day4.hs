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
                                     try)
import qualified Text.Parsec        as P ((<|>))
import           Text.Parsec.Text   (Parser)
import           Text.Read          (readMaybe)

type Entries = [Int]

type Table = [OSet Int]

partOneSampleSolution :: Solution
partOneSampleSolution =
  evaluate
    inputParser
    partOne
    (Label "Part One Sample")
    "src/Solutions/Day4/sample_input.txt"

checkTables :: Entries -> Table -> Maybe (Set Int, [Int])
checkTables entries =
  foldr (\curSet -> (<|>) (checkTable (OSet.toSet curSet) [] entries)) Nothing

checkTable :: Set Int -> Entries -> Entries -> Maybe (Set Int, [Int])
checkTable set [] [] = Nothing
checkTable set [] (entry:next) = checkTable set [entry] next
checkTable set (called:prevEntries) nextEntries =
  let curEntries = (called : prevEntries)
   in if set `isSubsetOf` Set.fromList curEntries
        then Just (set, curEntries)
        else case nextEntries of
               []           -> Nothing
               (entry:next) -> checkTable set (entry : curEntries) next

scoreTable :: Entries -> Table -> Int
scoreTable [] _ = 0
scoreTable (calledEntry:entries) table =
  let entries' = Set.fromList entries
      table' = Set.fromList $ table >>= toList
      unmarkedValue =
        getSum $ (foldMap Sum . toList) $ difference entries' table'
      marked = union entries' table'
   in unmarkedValue * calledEntry

partOne :: (Entries, [Table]) -> Text
partOne (entries, tables) =
  let grades =
        catMaybes $
        (\table -> (table, ) <$> checkTables entries table) <$> tables
      winner =
        foldr
          (\cur champion ->
             let (table, (_, calledEntries)) = cur
              in if length calledEntries < maybe 0 (length . snd . snd) champion
                   then Just cur
                   else champion)
          (grades !!? 0)
          grades
   in show $ (\(table, (_, entries)) -> scoreTable entries table) <$> winner

{- PARSING -}
tableParser :: Parser Table
tableParser = do
  row <- OSet.fromList <$> rowParser
  nextRows <- try (newline >> noneOf "\n" >> tableParser) P.<|> pure []
  let tableRows = row : nextRows
  pure $ appendColumnSets tableRows
  where
    rowParser :: Parser [Int]
    rowParser = do
      void (many1 (char ' ')) P.<|> pure ()
      val <- many1 (noneOf " \n") >>= parseInt "Row value is not an int"
      next <- (many1 (char ' ') >> rowParser) P.<|> mempty
      pure (val : next)
    appendColumnSets :: Table -> Table
    appendColumnSets tableRows =
      let tableColumns =
            fmap OSet.fromList $
            toList $
            mapWithIndex
              (\colIdx _ -> (!! colIdx) . toList <$> tableRows)
              (Seq.fromList tableRows)
       in tableRows <> tableColumns

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

parseInt :: [Char] -> [Char] -> ParsecT Text () Identity Int
parseInt err = maybe (fail err) pure . readMaybe
