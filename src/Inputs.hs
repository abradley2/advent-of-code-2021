{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Inputs
  ( module Inputs.Routes
  , module Inputs
  ) where

import           Inputs.Routes
import           Layout
import           Yesod

postNewInputsR :: Yesod master => SubHandlerFor Inputs master Html
postNewInputsR = do
  liftHandler $
    defaultLayout $
    layout
      [hamlet|
            <h3>You did it!
        |]
      (toWidget [cassius||])

getNewInputsR :: Yesod master => SubHandlerFor Inputs master Html
getNewInputsR = do
    $logInfo "\n\nHELLO THERE\n\n"
    liftHandler $
        defaultLayout $
        layout
            [hamlet|
                <h3>Get new inputs!
            |]
            (toWidget [cassius||])

instance Yesod master => YesodSubDispatch Inputs master where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesInputs)
