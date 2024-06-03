{-# LANGUAGE OverloadedStrings #-}
module Api.Templates.User where

import Models (User (userName))
import Lucid (Html, div_, class_, p_, ToHtml (toHtml))

renderUsers :: [User] -> Html () 
renderUsers [] = div_ [class_ "users"] (p_ "No data")
renderUsers users = div_ [class_ "users"] (do mapM_ renderUser users) 
  where renderUser user = p_ (toHtml (userName user))
  
