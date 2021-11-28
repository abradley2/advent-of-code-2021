{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Inputs.Routes where

import           Yesod

data Inputs =
  Inputs

mkYesodSubData
  "Inputs"
  [parseRoutes|
/ NewInputsR POST GET
|]
