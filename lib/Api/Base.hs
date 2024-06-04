{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Base where

import Lucid (Html, body_, charset_, doctypehtml_, head_, href_, link_, meta_, rel_, title_, type_)

import Servant (
  Capture,
  Get,
  HasServer (ServerT),
  JSON,
  Post,
  Proxy (..),
  ReqBody,
  err404,
  throwError,
  type (:<|>) (..),
  type (:>),
 )

import Api.Templates.Base.Footer (renderFooter)
import Servant.API.ContentTypes.Lucid (HTML)

{- | The base template for all pages.
This function takes the title of the page and the body of the page.
It returns a full HTML document.
The body of the page should be an HTML fragment.
-}
baseTemplate :: Html () -> Html () -> Html ()
baseTemplate title' body' = do
  doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ title'
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/style.min.css"]
    body_ $ do
      body'
      renderFooter

type BaseAPI = "/base" :> Get '[HTML] (Html ())
