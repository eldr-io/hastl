{-# LANGUAGE AllowAmbiguousTypes #-}

module Api.Templates.Guest.Guest where

import Api.Templates.Helpers.Htmx (hxExt_, hxPost_, hxSwap_, hxTarget_)
import Data.Text qualified as T
import Database.Persist (Entity (entityKey, entityVal))
import Database.Persist.Postgresql (fromSqlKey)
import Lucid (
  Html,
  ToHtml (toHtml),
  alt_,
  button_,
  class_,
  div_,
  form_,
  h3_,
  id_,
  img_,
  input_,
  li_,
  name_,
  p_,
  placeholder_,
  src_,
  type_,
  ul_,
 )
import Models (Guest (guestName))

renderAddGuestForm :: Html ()
renderAddGuestForm = do
  div_ [id_ "form-errors", class_ ""] mempty
  div_
    [ id_ "add-guest-form"
    , class_ "add-guest-form shadow-md rounded-md overflow-hidden max-w-fit mx-auto mt-16"
    ]
    $ do
      div_ [class_ "bg-gray-100 px-4 py-2"] $ p_ "Add a Party Guest"
      form_ [hxPost_ "/guests", hxTarget_ "#guests-ul", hxSwap_ "afterbegin transition:true", hxExt_ "json-enc"] $ do
        input_
          [ class_ "w-full px-4 py-2 border border-gray-200 rounded-md"
          , type_ "text"
          , name_ "name"
          , placeholder_ "Name"
          ]
        input_
          [ class_ "w-full px-4 py-2 border border-gray-200 rounded-md"
          , type_ "email"
          , name_ "email"
          , placeholder_ "Email"
          ]
        button_
          [ class_ "w-full px-4 py-2 bg-primary hover:bg-violet-500 text-white rounded-md mt-2"
          , type_ "submit"
          ]
          "Invite guest to your party"

renderGuestsComponent :: [Entity Guest] -> Html ()
renderGuestsComponent guests =
  div_
    [ id_ "guests"
    , class_ "guests bg-white shadow-md rounded-md overflow-hidden max-w-xl mx-auto mt-14"
    ]
    $ do
      div_ [class_ "bg-gray-100 px-4 py-2"] $ p_ "Party Goers"
      renderGuests guests

renderGuest :: Entity Guest -> Html ()
renderGuest guest = do
  let guestId = show (fromSqlKey (entityKey guest))
      name = guestName (entityVal guest)
  li_ [class_ "flex items-center px-6 py-4 smooth"] $ do
    img_
      [ class_ "w-12 h-12 rounded-full object-cover mr-4"
      , src_ ("https://randomuser.me/api/portraits/women/" <> T.pack guestId <> ".jpg")
      , alt_ "Guest"
      ]
    div_ [class_ "flex-1"] $ do
      h3_ [class_ "text-lg font-medium text-gray-800"] (toHtml name)
      p_ [class_ "text-sm font-normal text-gray-600"] "Party Animal"

renderGuests :: [Entity Guest] -> Html ()
renderGuests [] = ul_ [id_ "guests-ul", class_ "divide-y divide-gray-200"] $ li_ [] mempty
renderGuests guests = do
  ul_ [id_ "guests-ul", class_ "divide-y divide-gray-200"] $ do
    mapM_ renderGuest guests
