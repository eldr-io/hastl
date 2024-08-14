{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Guest where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (logDebugNS, logErrorNS)
import Database.Persist.Postgresql (
  Entity (..),
  getEntity,
  insert,
  selectList,
 )
import Servant (
  Get,
  HasServer (ServerT),
  JSON,
  Post,
  Proxy (..),
  ReqBody,
  ServerError (errBody, errHTTPCode, errHeaders),
  err500,
  throwError,
  type (:<|>) (..),
  type (:>),
 )

import Api.Templates.Guest.Guest (renderGuest, renderGuestsComponent)
import Config (AppT (..))
import Data.Aeson (FromJSON)
import Data.Text (Text, pack)
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import Lucid (Html, ToHtml (toHtml), class_, div_, id_, renderBS)
import Models (Guest (Guest), runDb, tryRunDb)
import Servant.API.ContentTypes.Lucid (HTML)

data CreateGuestPayload = CreateGuestPayload
  { name :: Text
  , email :: Text
  }
  deriving (Generic)

instance FromJSON CreateGuestPayload

type GuestAPI =
  "guests" :> Get '[HTML] (Html ())
    :<|> "guests" :> ReqBody '[JSON] CreateGuestPayload :> Post '[HTML] (Html ())

guestApi :: Proxy GuestAPI
guestApi = Proxy

-- | The server that runs the guestAPI
guestServer :: (MonadIO m) => ServerT GuestAPI (AppT m)
guestServer = allGuests :<|> createGuest

-- | Returns all guests in the database.
allGuests :: (MonadIO m) => AppT m (Html ())
allGuests = do
  logDebugNS "web" "allGuests"
  guests :: [Entity Guest] <- runDb (selectList [] [])
  return $ renderGuestsComponent guests

-- | Creates a guest in the database.
createGuest :: (MonadIO m) => CreateGuestPayload -> AppT m (Html ())
createGuest u = do
  logDebugNS "web" "creating a guest"
  time <- liftIO getCurrentTime
  result <- tryRunDb (insert (Guest (name u) (email u) time))
  case result of
    Left exception -> do
      logErrorNS "web" (Data.Text.pack (show exception))
      throwError $
        err500
          { errHeaders =
              [ ("HX-Retarget", "#form-errors")
              , ("HX-Reswap", "outerHTML")
              , ("Access-Control-Allow-Origin", "*") -- Enable CORS
              ]
          , errBody =
              renderBS $
                div_
                  [ id_ "form-errors"
                  , class_ "max-w-2lg mx-auto mt-2 flex inline-flex justify-between bg-red-100 border border-red-400 text-red-700 my-2 rounded "
                  ]
                  (toHtml (show exception))
          , errHTTPCode = 200 -- This is a hack to make sure htmx displays our error
          }
    Right key -> do
      logDebugNS "web" "guest created"
      maybeGuest <- runDb (getEntity key)
      case maybeGuest of
        Nothing -> do
          logErrorNS
            "web"
            "Failed to create guest"
          throwError err500
        Just guest ->
          return $ renderGuest guest
