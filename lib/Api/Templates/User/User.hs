{-# LANGUAGE AllowAmbiguousTypes #-}

module Api.Templates.User.User where

import Api.Templates.Helpers.Htmx (hxExt_, hxPost_, hxSwap_, hxTarget_)
import Data.Text qualified as T
import Database.Persist (Entity (entityKey, entityVal))
import Database.Persist.Postgresql (fromSqlKey)
import Lucid
import Models (User (userName))

renderAddUserForm :: Html ()
renderAddUserForm =
  div_
    [ id_ "add-user-form"
    , class_ "add-user-form bg-white shadow-md rounded-md overflow-hidden max-w-lg mx-auto mt-16"
    ]
    $ do
      div_ [class_ "bg-gray-100 px-4 py-2"] $ p_ "Add a Party Animal"
      form_ [hxPost_ "/users", hxTarget_ "#users-ul", hxSwap_ "beforeend transition:true", hxExt_ "json-enc"] $ do
        input_ [class_ "w-full px-4 py-2 border border-gray-200 rounded-md", type_ "text", name_ "name", placeholder_ "Name"]
        input_ [class_ "w-full px-4 py-2 border border-gray-200 rounded-md", type_ "email", name_ "email", placeholder_ "Email"]
        button_ [class_ "w-full px-4 py-2 bg-blue-500 text-white rounded-md mt-2", type_ "submit"] "Add Party Animal"

renderUsersComponent :: [Entity User] -> Html ()
renderUsersComponent users =
  div_
    [ id_ "users"
    , class_ "users bg-white shadow-md rounded-md overflow-hidden max-w-lg mx-auto mt-16"
    ]
    $ do
      div_ [class_ "bg-gray-100 px-4 py-2"] $ p_ "Party Goers"
      renderUsers users

renderUser :: Entity User -> Html ()
renderUser user = do
  let userId = show (fromSqlKey (entityKey user))
      name = userName (entityVal user)
  li_ [class_ "flex items-center px-6 py-4"] $ do
    img_
      [ class_ "w-12 h-12 rounded-full object-cover mr-4"
      , src_ ("https://randomuser.me/api/portraits/women/" <> T.pack userId <> ".jpg")
      , alt_ "User"
      ]
    div_ [class_ "flex-1"] $ do
      h3_ [class_ "text-lg font-medium text-gray-800"] (toHtml name)
      p_ [class_ "text-sm font-normal text-gray-600"] "Party Animal"

renderUsers :: [Entity User] -> Html ()
renderUsers [] = p_ "No guests yet..."
renderUsers users = do
  ul_ [id_ "users-ul", class_ "divide-y divide-gray-200"] $ do
    mapM_ renderUser users
