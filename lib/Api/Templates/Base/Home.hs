module Api.Templates.Base.Home where

import Api.Templates.Helpers.Htmx (hxGet_, hxSwap_, hxTrigger_)
import Api.Templates.User.User (renderAddUserForm)
import Data.Text (Text)
import Lucid (
  Html,
  a_,
  b_,
  br_,
  class_,
  div_,
  h1_,
  href_,
  img_,
  p_,
  src_,
  target_,
 )

renderHome :: Lucid.Html ()
renderHome =
  div_ [class_ "mt-4"] $ do
    div_
      [class_ "max-w-screen-xl mx-auto"]
      $ do
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
        renderBadges
        p_ [class_ "text-gray-600 mt-5"] "Try it out below by adding guests to your awesome party!"
        renderAddUserForm
        p_ [class_ "text-xs text-gray-500 mt-2 mx-auto text-center"] "* Don't worry, we won't actually send anything!"
    div_ [hxGet_ "/users", hxSwap_ "innerHTML", hxTrigger_ "load"] $ p_ "Loading..."

renderBadge :: Text -> Html ()
renderBadge src = img_ [class_ "col-span-1 inline", src_ src]

renderBadges :: Html ()
renderBadges =
  div_ [class_ "mx-auto max-w-screen-xl"] $ do
    mapM_ renderBadge badges
 where
  badges =
    [ "https://img.shields.io/badge/haskell-5D4F85?style=for-the-badge&logo=haskell&logoColor=white"
    , "https://img.shields.io/badge/htmxjs-3366CC?style=for-the-badge&logo=htmx&logoColor=white"
    , "https://img.shields.io/badge/alpinejs-8BC0D0?style=for-the-badge&logo=alpine.js&logoColor=white"
    , "https://img.shields.io/badge/Servant-5D4F85?style=for-the-badge&logo=haskell&logoColor=white"
    , "https://img.shields.io/badge/Tailwind-06B6D4?style=for-the-badge&logo=tailwindcss&logoColor=white"
    , "https://img.shields.io/badge/Lucid-5D4F85?style=for-the-badge&logo=haskell&logoColor=white"
    ]
