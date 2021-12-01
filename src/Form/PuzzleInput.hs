{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Form.PuzzleInput
  ( form
  , day
  , slug
  , content
  ) where

import           Relude
import           Yesod      (HandlerFor, Html, RenderMessage, WidgetFor, Yesod)
import           Yesod.Form

data Form =
  Form
    { day     :: Int
    , slug    :: Text
    , content :: Textarea
    }
  deriving (Show)

form ::
     Yesod site
  => RenderMessage site FormMessage =>
       Int -> Html -> MForm (HandlerFor site) ( FormResult Form
                                              , WidgetFor site ())
form day =
  renderDivs $
  Form <$> areq Yesod.Form.hiddenField "" (Just day) <*>
  areq Yesod.Form.textField "slug" Nothing <*>
  areq Yesod.Form.textareaField "content" Nothing
