{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.PuzzleInput where

import           Database.Selda as Selda (Generic, SqlRow, Table, Text, table)
import           Relude

data Row =
  Row
    { day     :: Int
    , content :: Text
    , slug    :: Text
    }
  deriving (Generic)

instance SqlRow Row

table :: Table Row
table = Selda.table "puzzle_input" []
