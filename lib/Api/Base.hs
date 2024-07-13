{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Base where

import Lucid (
  Html,
  body_,
  class_,
  div_,
  doctypehtml_,
  id_,
 )
import Servant (
  Get,
  HasServer (ServerT),
  Proxy (..),
  (:<|>),
  (:>),
  type (:<|>) (..),
 )

import Api.Templates.Base.About (renderAbout)
import Api.Templates.Base.Footer (renderFooter)
import Api.Templates.Base.Header (renderBanner, renderHeader, renderNavigation)
import Api.Templates.Base.Home (renderHome)
import Api.Templates.Helpers.Alpine (useAlpine)
import Api.Templates.Helpers.Htmx (useHtmxJsExt, useHtmxVersion)
import Config (AppT)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Servant.API.ContentTypes.Lucid (HTML)

baseTemplate :: Html () -> Text -> Html () -> Html ()
baseTemplate title pageName content = do
  doctypehtml_ $ do
    renderHeader title
    body_ $ do
      renderBanner
      renderNavigation pageName
      div_ [id_ "errors", class_ "max-w-screen-xl mx-auto"] mempty
      content
      renderFooter
      useHtmxVersion (1, 9, 12)
      useHtmxJsExt
      useAlpine

type BaseAPI =
  Get '[HTML] (Html ())
    :<|> "about" :> Get '[HTML] (Html ())

baseApi :: Proxy BaseAPI
baseApi = Proxy

-- | The server that runs the UserAPI
baseServer :: (MonadIO m) => ServerT BaseAPI (AppT m)
baseServer = base :<|> about

about :: (MonadIO m) => AppT m (Html ())
about =
  return $
    baseTemplate
      "Hastl | About"
      "About"
      renderAbout

base :: (MonadIO m) => AppT m (Html ())
base =
  return $
    baseTemplate
      "Hastl | Modern Haskell Web Application Starter Kit"
      "Home"
      renderHome
