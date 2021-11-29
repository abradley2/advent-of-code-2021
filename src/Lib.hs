{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Lib
  ( runApp
  ) where

import qualified DB                     (setup)
import qualified DB.PuzzleInput         as PuzzleInput (Row (..), content,
                                                        table)
import           Database.Selda         (Query, Relational, Row, SeldaT, SqlRow,
                                         Table, from, fromSql, query, table,
                                         tryCreateTable, (!))
import qualified Database.Selda         as Form
import qualified Database.Selda         as Selda (insert, select)
import           Database.Selda.Backend (SeldaBackend, SeldaConnection,
                                         runSeldaT)
import           Database.Selda.SQLite  as SQLite
import           Layout
import           Relude
import           Yesod
import           Yesod.Form
import           Yesod.Static

data App =
  App
    { appStatic :: Static
    , runQuery  :: forall a. SeldaT SQLite IO a -> IO a
    }

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

-- Derive routes and instances for App.
mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
/day/#DayID DayR GET POST
/static StaticR Static appStatic
|]

instance Yesod App

newtype DayID =
  DayID Int
  deriving (Eq, Show, Read, PathPiece)

data PuzzleInputFields =
  PuzzleInputFields
    { day     :: Int
    , slug    :: Text
    , content :: Text
    }
  deriving (Show)

getHomeR :: HandlerFor App Html
getHomeR = do
  app <- getYesod
  result <-
    liftIO $
    runQuery app $
    query $ do
      inputs <- Selda.select PuzzleInput.table
      pure inputs
  defaultLayout $
    layout
      [hamlet|
        <div>Hello there
          <div>
            $forall row <- result
              <h3>#{PuzzleInput.content row}
        |] $
    toWidget
      [cassius|
          |]

postDayR :: DayID -> HandlerFor App Html
postDayR (DayID day) = do
  app <- getYesod
  slug' <- lookupPostParam "slug"
  content' <- lookupPostParam "content"
  result <-
    case PuzzleInputFields day <$> slug' <*> content' of
      Just form ->
        liftIO $
        runQuery app $ do
          let row = PuzzleInput.Row day (slug form) (content form)
          Selda.insert PuzzleInput.table [row]
          pure
            [hamlet|
              <div>Everything looks good here chief!
            |]
      Nothing ->
        pure
          [hamlet|
            <div>There was an erorr submitting stuff!
          |]
  defaultLayout $ layout result $ toWidget [lucius||]

getDayR :: DayID -> HandlerFor App Html
getDayR (DayID day) = do
  app <- getYesod
  slug' <- lookupPostParam "slug"
  content' <- lookupPostParam "content"
  let submittedForm = PuzzleInputFields day <$> slug' <*> content'
  _ <-
    case submittedForm of
      Just form ->
        liftIO $
        runQuery app $ do
          let row = PuzzleInput.Row day (slug form) (content form)
          Selda.insert PuzzleInput.table [row]
          pure ()
      _ -> pure ()
  defaultLayout $
    layout
      [hamlet|
          <div>Day page with inputs #{day}
            <form action="/day/#{day}" method="POST">
              <input id="day" name="day" value="#{day}" >
              <label for="slug">Slug
              <input id="slug" name="slug" type="text">
              <br>
              <label for="content">Content
              <textarea id="content" name="content">
              <br>
              <button type="submit">Submit
        |] $
    toWidget
      [lucius|
          #day {
            display: none;
          }
        |]

runApp :: IO ()
runApp = do
  staticFiles <- static "static"
  con <- sqliteOpen "data.db"
  _ <- runSeldaT DB.setup con
  liftIO $ warp 3000 App {appStatic = staticFiles, runQuery = (`runSeldaT` con)}
