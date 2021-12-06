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

type Table = [OSet Int]

partOneSampleSolution :: Solution
partOneSampleSolution =
  evaluate
    inputParser
    partOne
    (Label "Part One Sample")
    "src/Solutions/Day4/sample_input.txt"

checkTableSets :: Entries -> Table -> Maybe (Set Int, [Int])
checkTableSets entries =
  foldr
    (\curSet -> (<|>) (checkTableSet (OSet.toSet curSet) [] entries))
    Nothing

checkTableSet :: Set Int -> Entries -> Entries -> Maybe (Set Int, [Int])
checkTableSet set curEntries nextEntries =
  if set `isSubsetOf` Set.fromList curEntries
    then Just (set, curEntries)
    else case nextEntries of
           []           -> Nothing
           (entry:next) -> checkTableSet set (entry : curEntries) next

partOne :: (Entries, [Table]) -> Text
partOne (entries, tables) =
  let grades = catMaybes $ checkTableSets entries <$> tables
      winner =
        foldr
          (\cur champion ->
             if length (snd cur) < maybe 0 (length . snd) champion
               then Just cur
               else Just $ fromMaybe cur champion)
          (grades !!? 0)
          grades
   in show winner

checkTable :: Table -> Maybe Entries
checkTable table = Nothing

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
