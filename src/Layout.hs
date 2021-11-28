{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Layout (layout) where

import           Relude
import           Yesod

days :: [Text]
days = show <$> [1 .. 25]

layout :: MonadWidget m => HtmlUrl (Route (HandlerSite m)) -> m b -> m b
layout body widget = do
  toWidget
    [hamlet|
        <div .layout>
            <div .layout__navbar>
                <span .navbar-item.navbar-item--display>
                    DAY
                $forall day <- days
                    <a .navbar-item href="/day/#{day}">
                        #{day}
            <div .layout__body>
                ^{body}
    |]
  toWidgetHead
    [lucius|
        @font-face {
            font-family: 'Clacon';
            src: url('/static/clacon2.woff2') format('woff2');
        }
        :root {
            --keyword-green: #009900;
            --active-green: #99ff99;
            --text-gray: #cccccc;
            --hover-gray: rgba(119,119,165,.2);

            --shadow-glow: 0 0 2px #99ff99, 0 0 5px #99ff99;
        }
        body {
            margin: 0px;
            padding: 0px;
            letter-spacing: 4px;
            color: var(--text-gray);
        }
        .layout {
            display: flex;
            width: 100vw;
            height: 100vh;
            overflow: hidden;
            background-color: #0f0f23;
            font-family: Clacon;
        }
        .layout__navbar {
            flex: 0;
            min-width: 64px;
            border-right: 1px solid var(--keyword-green);
            font-size: 16px;
            font-weight: 600;
            padding-top: 4px;
            box-shadow: var(--shadow-glow);
        }
        .navbar-item {
            display: block;
            cursor: pointer;
            text-align: center;
            line-height: 36px;
            height: 32px;
            color: var(--text-gray);
            text-decoration: none;
        }
        .navbar-item:hover, .navbar-item:focus {
            background-color: var(--hover-gray);
        }
        .navbar-item--display {
            cursor: initial;
        }
        .navbar-item--display:hover, .navbar-item--display:focus {
            background-color: transparent;
        }
        .layout__body {
            flex: 1000;
            padding: 16px;
        }  
    |]
  widget
