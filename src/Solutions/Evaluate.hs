{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Solutions.Evaluate
  ( evaluate
  ) where

import           Relude
import           Text.Parsec
import           Text.Parsec.Text

evaluate :: Show b => Parser a -> FilePath -> (a -> b) -> IO Text
evaluate parser filePath solve = do
  rawInput <- readFileText filePath
  let input = parse parser filePath rawInput
  case input of
    Left err          -> pure $ show err
    Right parsedInput -> pure $ show $ solve parsedInput
