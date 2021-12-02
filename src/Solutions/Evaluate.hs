{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Solutions.Evaluate
  ( evaluate
  , Label(..)
  , Solution
  ) where

import           Relude
import           Text.Parsec
import           Text.Parsec.Text

newtype Label =
  Label Text

type Solution = IO (Text, Text)

evaluate :: Show b => Parser a -> (a -> b) -> Label -> FilePath -> Solution
evaluate parser solve (Label label) filePath = do
  rawInput <- readFileText filePath
  let input = parse parser filePath rawInput
  pure $
    (, label) $
    case input of
      Left err          -> show err
      Right parsedInput -> show $ solve parsedInput
