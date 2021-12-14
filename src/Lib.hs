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

import           Layout             (layout)
import           Relude
import           Solutions.Day1     as Day1
import           Solutions.Day10    as Day10
import           Solutions.Day2     as Day2
import           Solutions.Day3     as Day3
import           Solutions.Day4     as Day4
import           Solutions.Day5     as Day5
import           Solutions.Day6     as Day6
import           Solutions.Evaluate (Solution)
import           Yesod
import           Yesod.Static

getSolutions day =
  mapM
    liftIO
    (case day of
       1 -> [Day1.partOneSolution, Day1.partTwoSolution]
       2 -> [Day2.partOneSolution, Day2.partTwoSolution]
       3 ->
         [ Day3.partOneSampleSolution
         , Day3.partOneSolution
         , Day3.partTwoSampleSolution
         , Day3.partTwoSolution
         ]
       4 -> [Day4.partOneSampleSolution, Day4.partOneSolution]
       5 ->
         [ Day5.partOneSampleSolution
         , Day5.partOneSolution
         , Day5.partTwoSampleSolution
         , Day5.partTwoSolution
         ]
       6 ->
         [ Day6.partOneSampleSolution
         , Day6.partOneSolution
         , Day6.partTwoSampleSolution
         , Day6.partTwoSolution
         ]
       10 -> [Day10.partOneSampleSolution, Day10.partOneSolution]
       _ -> [])

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
  solutions <- getSolutions day
  layout $ do
    [whamlet|
      <div>Day <b>#{day}
      $forall (answer, label) <- solutions
        <div class="label-answer-group">
          <div class="label">#{label}
          <pre class="answer">#{answer}
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
