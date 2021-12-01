{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Lib
  ( runApp
  ) where

import           Layout         (layout)
import           Relude
import qualified Solutions.Day1 as Day1 (evaluatePartOne, evaluatePartTwo)
import           Yesod
import           Yesod.Static

data App =
  App
    { appStatic :: Static
    }

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
/day/#DayID DayR GET
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

getDayR :: DayID -> HandlerFor App Html
getDayR (DayID day) = do
  app <- getYesod
  solutions <-
    case day of
      1 -> sequence $ liftIO <$> [Day1.evaluatePartOne, Day1.evaluatePartTwo]
      _ -> pure []
  layout $ do
    [whamlet|
      <div>Day <b>#{day}
      $forall (answer, label) <- solutions
        <div class="label-answer-group">
          <h3 class="label">#{label}
          <span class="answer">#{answer}
    |]
    toWidget
      [lucius|
        .label-answer-group {
          margin-top: 24px;
        }

        .label {
          font-size: 20px;
          color: var(--keyword-green);
        }

        .answer {
          font-size: 16px;
          color: var(--active-green);
          text-shadow: var(--shadow-glow);
        }
      |]

runApp :: IO ()
runApp = do
  staticFiles <- static "static"
  liftIO $ warp 3000 App {appStatic = staticFiles}
