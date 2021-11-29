{-# LANGUAGE OverloadedStrings #-}

module DB where

import qualified DB.PuzzleInput        as PuzzleInput (Row, table)
import           Database.Selda        (SeldaT, tryCreateTable)
import           Database.Selda.SQLite (SQLite)

setup :: SeldaT SQLite IO ()
setup = do
  _ <- tryCreateTable PuzzleInput.table
  pure ()
