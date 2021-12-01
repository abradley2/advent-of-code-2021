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
import           Data.List              (length)
import           Database.Selda         (Col (..), Query, Relational, Row,
                                         SeldaT, SqlRow, SqlType (mkLit), Table,
                                         from, fromSql, tryCreateTable, (!),
                                         (.==))
import qualified Database.Selda         as Selda (Col (..), SqlType, insert,
                                                  int, query, restrict, select)
import           Database.Selda.Backend (Lit (LInt), SeldaBackend,
                                         SeldaConnection, runSeldaT)
import           Database.Selda.SQLite  as SQLite
import           Form.CheckboxGroup     (initCheckboxes)
import qualified Form.PuzzleInput       as PuzzleInputForm (content, day, form,
                                                            slug)
import           Layout                 (layout)
import           Relude
import qualified Solutions.Day1         as Day1 (solvePartOne, solvePartTwo)
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
          Selda.insert
            PuzzleInput.table
            [ PuzzleInput.Row
                (PuzzleInputForm.day puzzleInput)
                (unTextarea $ PuzzleInputForm.content puzzleInput)
                (PuzzleInputForm.slug puzzleInput)
            ]
          pure ()
      redirect ("/day/#{day}" :: Text)
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
  solution <-
    case day of
      1 -> liftIO Day1.solvePartTwo
      _ -> pure ""
  inputs <-
    liftIO $
    runQuery app $
    Selda.query $ do
      res <- Selda.select PuzzleInput.table
      Selda.restrict (res ! #day .== Selda.int day)
      pure res
  layout $ do
    [whamlet|
      <div>Day
        <b>#{day}
        <h3>#{solution}
        <div class="encouragement">
          <div class="encouragement__title">Daily encouragement:
          <div class="encouragement__message">You look nice today
        <div class="puzzle-inputs">
          $if length inputs == 0
            <div class="no-inputs-available">No inputs available
          $forall input <- inputs
            <form class="checkbox-group" action="/day/#{day}" method="DELETE">
              <input class="hidden" type="text" name="slug" value="#{PuzzleInput.slug input}">
              <input class="hidden" type="radio" id="#{PuzzleInput.slug input}" name="slug" value="#{PuzzleInput.slug input}">
              <label for="#{PuzzleInput.slug input}" tabindex="0">
                <span class="checkbox-group__checkbox">
                <span class="checkbox-group__label">
                  <span>#{PuzzleInput.slug input}
          <div class="footer">
            $if length inputs /= 0
              <button class="eval-button">Evaluate (Part One)
              <button class="eval-button">Evaluate (Part Two)
        <form action="/day/#{day}" enctype=#{enctype} method="POST">
          ^{widget}
          <button type="submit">Add Puzzle Input
    |]
    initCheckboxes ".puzzle-inputs"
    toWidgetHead
      [lucius|
        .encouragement {
          max-width: 240px;
          margin: 16px 0px;
          border: 1px dashed var(--text-gray);
          padding: 8px;
          background-color: var(--background-black);
        }
        .encouragement__title {
          color: var(--active-green);
          margin-bottom: 8px;
          font-size: 16px;
        }
        .encouragement__message {
          font-size: 14px;
        }
        .footer {
        }
        .eval-button {
          margin-top: 16px;
          margin-right: 16px;
        }
        .no-inputs-available {
        }
        .puzzle-inputs {
          margin: 32px 0px;
        }
        .checkbox-group > label {
          display: inline-flex;
          cursor: pointer;
          margin: 8px 0px;
          border: 1px solid transparent;
          align-items: center;
        }
        .checkbox-group > label:hover, .checkbox-group > label:focus {
          outline: 1px solid var(--active-green);
          outline-offset: 2px;
        }
        .checkbox-group__label {
          display: inline-block;
          line-height: 16px;
          font-size: 16px;
          color: var(--keyword-green);
          padding-left: 8px;
          max-width: 200px;
        }
        .checkbox-group__checkbox {
          box-sizing: border-box;
          height: 24px;
          width: 24px;
          padding: 6px;
          border: 1px solid var(--keyword-green);
          display: block;
          flex-grow: 0;
          flex-shrink: 0;
        }
        .checkbox__checkmark {
          display: block;
          width: 100%;
          height: 100%;
          background-color: var(--active-green);
          box-shadow: var(--shadow-glow);
        }
      |]

runApp :: IO ()
runApp = do
  staticFiles <- static "static"
  con <- sqliteOpen "data.db"
  _ <- runSeldaT DB.setup con
  liftIO $ warp 3000 App {appStatic = staticFiles, runQuery = (`runSeldaT` con)}
