{-# LANGUAGE DataKinds #-}

module Api.Base where

import Lucid
import Servant (
  Get,
  HasServer (ServerT),
  Proxy (..),
 )

import Api.Templates.Base.Footer (renderFooter)
import Config (AppT)
import Control.Monad.Cont (MonadIO)
import Servant.API.ContentTypes.Lucid (HTML)
import Api.Templates.Helpers.Htmx (useHtmxVersion)
import Api.Templates.Helpers.Alpine (useAlpine)

baseTemplate :: Html () -> Html () -> Html ()
baseTemplate title' body' = do
  doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ title'
      link_ [rel_ "stylesheet", type_ "text/css", href_ "./css/style.min.css"]
    body_ $ do
      body'
      renderFooter
      useHtmxVersion (1,9,12)
      useAlpine

type BaseAPI = Get '[HTML] (Html ())

baseApi :: Proxy BaseAPI
baseApi = Proxy

-- | The server that runs the UserAPI
baseServer :: (MonadIO m) => ServerT BaseAPI (AppT m)
baseServer = base

base :: (MonadIO m) => AppT m (Html ())
base = return $ baseTemplate "hastl" (div_ [class_ "mt-4"] (p_ "body"))
