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
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import Lucid (Html, ToHtml (toHtml), class_, div_, id_, renderBS, p_, ul_, li_)
import Models (Guest (Guest), runDb, tryRunDb)
import Servant.API.ContentTypes.Lucid (HTML)
import Text.Email.Validate (isValid)
import Data.Text.Encoding (encodeUtf8)

data CreateGuestPayload = CreateGuestPayload
  { name :: Text
  , email :: Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON CreateGuestPayload

-- | Validation errors for guest creation
data ValidationError 
  = EmptyName
  | EmptyEmail
  | InvalidEmail
  deriving (Show, Eq)

-- | Validate guest creation payload
validateGuest :: CreateGuestPayload -> Either [ValidationError] CreateGuestPayload
validateGuest payload = 
  let nameErrors = if T.null (T.strip (name payload)) then [EmptyName] else []
      emailErrors = if T.null (T.strip (email payload)) 
                   then [EmptyEmail] 
                   else if not (isValid (encodeUtf8 (email payload)))
                        then [InvalidEmail]
                        else []
      allErrors = nameErrors ++ emailErrors
  in if null allErrors then Right payload else Left allErrors

-- | Convert validation errors to user-friendly messages
validationErrorToText :: ValidationError -> Text
validationErrorToText EmptyName = "Name cannot be empty"
validationErrorToText EmptyEmail = "Email cannot be empty"
validationErrorToText InvalidEmail = "Please enter a valid email address"

-- | Render validation errors as HTML
renderValidationErrors :: [ValidationError] -> Html ()
renderValidationErrors errors = 
  div_
    [ id_ "form-errors"
    , class_ "max-w-2lg mx-auto mt-2 bg-red-100 border border-red-400 text-red-700 my-2 rounded p-3"
    ]
    $ do
      p_ [class_ "font-semibold mb-2"] "Please fix the following errors:"
      ul_ [class_ "list-disc list-inside space-y-1"] $ 
        mapM_ (\err -> li_ $ toHtml $ validationErrorToText err) errors

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
createGuest payload = do
  logDebugNS "web" "creating a guest"
  case validateGuest payload of
    Left validationErrors -> do
      logDebugNS "web" "validation failed"
      throwError $
        err500
          { errHeaders =
              [ ("HX-Retarget", "#form-errors")
              , ("HX-Reswap", "outerHTML")
              , ("Access-Control-Allow-Origin", "*") -- Enable CORS
              ]
          , errBody = renderBS $ renderValidationErrors validationErrors
          , errHTTPCode = 200 -- This is a hack to make sure htmx displays our error
          }
    Right validPayload -> do
      time <- liftIO getCurrentTime
      result <- tryRunDb (insert (Guest (name validPayload) (email validPayload) time))
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
                      , class_ "max-w-2lg mx-auto mt-2 bg-red-100 border border-red-400 text-red-700 my-2 rounded p-3"
                      ]
                      $ do
                        p_ [class_ "font-semibold"] "An error occurred:"
                        p_ $ toHtml $ if T.isInfixOf "duplicate key" (T.pack (show exception))
                                     then ("A guest with this email address already exists" :: Text)
                                     else ("Unable to create guest. Please try again." :: Text)
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
