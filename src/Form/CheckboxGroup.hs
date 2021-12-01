{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Form.CheckboxGroup
  ( initCheckboxes
  ) where

import           Relude
import           Yesod

initCheckboxes :: Yesod site => Text -> WidgetFor site ()
initCheckboxes querySelector =
  toWidget
    [julius|
        const inputs = document.querySelector(#{querySelector}).querySelectorAll('.checkbox-group')

        function getCheckmark () {
          const mark = document.createElement('span')
          mark.className = 'checkbox__checkmark'
          return mark
        }

        function handleInputSelect (el) {
          let checkmark = el.querySelector('checkbox__checkmark')
          if (checkmark) {
            checkmark.parentElement.removeChild(checkmark)
          } else {
            inputs.forEach(function (input) {
              input.querySelector('.checkbox-group__checkbox').innerHTML = ''
            })
            el.querySelector('.checkbox-group__checkbox').appendChild(getCheckmark())
          }
        }

        inputs.forEach(function (el) {
          el.querySelector('label').onclick = function () { handleInputSelect(el) }
        })
  |]
