{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedLabels #-}

module Lib
  ( runApp
  ) where

import           Relude

import           Database.Selda            (Query, Relational, Row, SeldaT,
                                            SqlRow, Table, fromSql, query,
                                            table, tryCreateTable, from, (!))
import qualified Database.Selda            as Selda (insert, select)
import           Database.Selda.Backend    (SeldaBackend, SeldaConnection,
                                            runSeldaT)
import           Database.Selda.Migrations as Migrations
import           Database.Selda.SQLite     as SQLite
import           Inputs
import           Layout
import           Yesod
import           Yesod.Static

data App =
  App
    { appStatic :: Static
    , getInputs :: Inputs
    , runQuery  :: forall a. SeldaT SQLite IO a -> IO a
    }

-- Derive routes and instances for App.
mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
/inputs InputsR Inputs getInputs
/static StaticR Static appStatic
|]

instance Yesod App

-- The handler for the GET request at /, corresponds to HomeRoute.
getHomeR :: HandlerFor App Html
getHomeR = do
  app <- getYesod
  result <- liftIO $ runQuery app $ query $ do 
    inputs <- #content `from` Selda.select puzzleInputTable
    pure $ inputs
  defaultLayout $
    layout
      [hamlet|
          <div>Hello there
        |] $
    toWidget
      [cassius|
          |]

doStuff :: SeldaT SQLite IO ()
doStuff = do
  pure ()

c :: IO ()
c = do
  con <- sqliteOpen "data.db"
  stuff <- runSeldaT doStuff con
  pure ()

b :: IO (SeldaConnection SQLite)
b = sqliteOpen "data.db"

runApp :: IO ()
runApp = do
  staticFiles <- static "static"
  con <- sqliteOpen "data.db"
  liftIO $
    warp
      3000
      App
        { appStatic = staticFiles
        , getInputs = Inputs
        , runQuery = (`runSeldaT` con)
        }

data EmptyRow =
  EmptyRow
    { day'     :: Int
    , content' :: ByteString
    }
  deriving (Generic)

instance SqlRow EmptyRow

emptyTable :: Table EmptyRow
emptyTable = table "puzzle_input" []

{-
Day Slug Blob
-}
data PuzzleInput =
  PuzzleInput
    { day     :: Int
    , content :: ByteString
    , slug    :: Text
    }
  deriving (Generic)

instance SqlRow PuzzleInput

puzzleInputTable :: Table PuzzleInput
puzzleInputTable = table "puzzle_input" []

-- type MigrationStep backend = [Migration backend]
-- Migration :: (Relational a, Relational b) => Table a -> Table b -> (Row backend a -> Query backend (Row backend b)) -> Migration backend
-- select :: Relational a => Table a -> Query s (Row s a)
type SQLBackend = (SeldaBackend SQLite)

-- a :: Migration (SeldaBackend SQLite)
-- a :: (Relational a, Relational b)
--   => Table a
--   -> Table b
--   -> (Row backend a -> Query backend (Row backend b))
--   -> Migration backend
-- a tableA tableB row =
--   select tableB
a :: Migration backend
a =
  Migration
    emptyTable
    puzzleInputTable
    (\emptyRow -> Selda.select puzzleInputTable)
-- select :: Relational a => Table a -> Query s (Row s a)
