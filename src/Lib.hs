{-# LANGUAGE DeriveGeneric              #-}
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
import           Database.Selda         (Col (..), Query, Relational, Row,
                                         SeldaT, SqlRow, SqlType (mkLit), Table,
                                         from, fromSql, tryCreateTable, (!),
                                         (.==))
import qualified Database.Selda         as Selda (Col (..), SqlType, insert,
                                                  int, query, restrict, select)
import           Database.Selda.Backend (Lit (LInt), SeldaBackend,
                                         SeldaConnection, runSeldaT)
import           Database.Selda.SQLite  as SQLite
import qualified Form.PuzzleInput       as PuzzleInputForm (content, day, form,
                                                            slug)
import           Layout                 (layout)
import           Relude
import           Yesod
import           Yesod.Form             as Form
import           Yesod.Static

data App =
  App
    { appStatic :: Static
    , runQuery  :: forall a. SeldaT SQLite IO a -> IO a
    }

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

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

getHomeR :: HandlerFor App Html
getHomeR = do
  app <- getYesod
  layout $ do
    [whamlet|
      <div>Select a day
    |]

postDayR :: DayID -> HandlerFor App Html
postDayR (DayID day) = do
  app <- getYesod
  ((result, widget), enctype) <- runFormPost $ PuzzleInputForm.form day
  case result of
    FormSuccess puzzleInput -> do
      _ <-
        liftIO $
        runQuery app $ do
          Selda.insert PuzzleInput.table []
          pure ()
      layout
        [whamlet|
          <div>
            <a href="/day/#{day}">Back to day #{day} page
        |]
    _ -> do
      layout
        [whamlet|
          <div>There was an error submitting the form
          <form action="/day/#{day}" enctype=#{enctype} method="POST">
            ^{widget}
            <button type="submit">Add Puzzle Input
        |]

getDayR :: DayID -> HandlerFor App Html
getDayR (DayID day) = do
  app <- getYesod
  (widget, enctype) <- generateFormPost $ PuzzleInputForm.form day
  inputs <-
    liftIO $
    runQuery app $
    Selda.query $ do
      res <- Selda.select PuzzleInput.table
      Selda.restrict (res ! #day .== Selda.int day)
      pure res
  layout $ do
    [whamlet|
      <div>Day page with inputs #{day}
        <div class="puzzle-inputs">
        $forall input <- inputs
          <div>
            <input class="hidden" type="checkbox" id="#{PuzzleInput.slug input}" name="#{PuzzleInput.slug input}">
            <label for="#{PuzzleInput.slug input}" class="puzzle-input">
              <span class="puzzle-input__checkbox puzzle-input__checkbox--checked">
              <span class="puzzle-input__label">#{PuzzleInput.slug input}
        <form action="/day/#{day}" enctype=#{enctype} method="POST">
          ^{widget}
          <button type="submit">Add Puzzle Input
    |]
    toWidget
      [lucius|
        .puzzle-inputs {

        }
        .puzzle-input {
          display: flex;
        }
        .puzzle-input__checkbox {
          height: 24px;
          width: 24px;
          border: 1px solid var(--keyword-green);
        }
        .puzzle-input__checkbox--checked {
          margin: 4px;
          width: 100%;
          height: 100%;
          background-color: var(--active-green);
        }
      |]

runApp :: IO ()
runApp = do
  staticFiles <- static "static"
  con <- sqliteOpen "data.db"
  _ <- runSeldaT DB.setup con
  liftIO $ warp 3000 App {appStatic = staticFiles, runQuery = (`runSeldaT` con)}
