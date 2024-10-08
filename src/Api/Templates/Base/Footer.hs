{-# LANGUAGE OverloadedStrings #-}

module Api.Templates.Base.Footer where

import Lucid (Html, a_, class_, div_, href_, p_)

renderFooter :: Html ()
renderFooter =
  div_
    [class_ "absolute inset-x-0 bottom-0 max-w-screen-lg text-center mx-auto footer text-gray-500 mb-5 text-sm"]
    ( p_ $ do
        "hastl - modern haskell web app template coded with <3 by "
        a_ [href_ "https://eldr.io"] "eldr.io"
    )
