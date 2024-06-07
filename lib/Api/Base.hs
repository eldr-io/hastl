{-# LANGUAGE DataKinds #-}

module Api.Base where

import Lucid (
  Html,
  a_,
  b_,
  body_,
  br_,
  class_,
  div_,
  doctypehtml_,
  h1_,
  href_,
  p_,
  script_,
  src_,
  target_,
 )
import Servant (
  Get,
  HasServer (ServerT),
  Proxy (..),
 )

import Api.Templates.Base.Footer (renderFooter)
import Api.Templates.Base.Header (renderBanner, renderHeader, renderNavigation)
import Api.Templates.Helpers.Alpine (useAlpine)
import Api.Templates.Helpers.Htmx (hxGet_, hxSwap_, hxTrigger_, useHtmxJsExt, useHtmxVersion)
import Api.Templates.User.User (renderAddUserForm)
import Config (AppT)
import Control.Monad.Cont (MonadIO)
import Servant.API.ContentTypes.Lucid (HTML)

baseTemplate :: Html () -> Html () -> Html ()
baseTemplate title content = do
  doctypehtml_ $ do
    renderHeader title
    body_ $ do
      renderBanner
      renderNavigation
      content
      renderFooter
      useHtmxVersion (1, 9, 12)
      useHtmxJsExt
      useAlpine

type BaseAPI = Get '[HTML] (Html ())

baseApi :: Proxy BaseAPI
baseApi = Proxy

-- | The server that runs the UserAPI
baseServer :: (MonadIO m) => ServerT BaseAPI (AppT m)
baseServer = base

base :: (MonadIO m) => AppT m (Html ())
base =
  return $
    baseTemplate
      "hastl"
      ( div_ [class_ "mt-4"] $ do
          div_ [class_ "max-w-screen-xl mx-auto"] $ do
            h1_ [class_ "text-3xl font-bold text-gray-900"] "Welcome to hastl"
            p_ [class_ "text-gray-600"] $ do
              "hastl is a modern "
              a_ [href_ "haskell.org", target_ "_blank"] "Haskell"
              " web application using "
              b_ "(H)tmx, "
              b_ "(A)lpine.js, "
              b_ "(S)ervant, "
              b_ "(T)ailwind-css "
              "and"
              b_ "(L)ucid. "
              "It is licensed under MIT and is entirely free and open source."
            br_ []
            p_ [class_ "text-gray-600"] "Try it out below by adding guests to your awesome party!"
            renderAddUserForm
          div_ [hxGet_ "/users", hxSwap_ "innerHTML", hxTrigger_ "load"] $ p_ "Loading..."
      )
