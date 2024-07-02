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
  Lucid.div_ [Lucid.class_ "mt-4"] $ do
    Lucid.div_
      [Lucid.class_ "max-w-screen-xl mx-auto"]
      $ do
        Lucid.h1_ [Lucid.class_ "text-3xl font-bold text-gray-900"] "Welcome to hastl"
        Lucid.p_ [Lucid.class_ "text-gray-600"] $ do
          "hastl is a modern "
          Lucid.a_ [Lucid.href_ "haskell.org", Lucid.target_ "_blank"] "Haskell"
          " web application using "
          Lucid.b_ "(H)tmx, "
          Lucid.b_ "(A)lpine.js, "
          Lucid.b_ "(S)ervant, "
          Lucid.b_ "(T)ailwind-css "
          "and"
          Lucid.b_ "(L)ucid. "
          "It is licensed under MIT and is entirely free and open source."
        Lucid.br_ []
        renderBadges
        Lucid.p_ [Lucid.class_ "text-gray-600 mt-5"] "Try it out below by adding guests to your awesome party!"
        renderAddUserForm
        Lucid.p_ [Lucid.class_ "text-xs text-gray-500 mt-2 mx-auto text-center"] "* Don't worry, we won't actually send anything!"
    Lucid.div_ [hxGet_ "/users", hxSwap_ "innerHTML", hxTrigger_ "load"] $ Lucid.p_ "Loading..."

renderBadge :: Text -> Lucid.Html ()
renderBadge src = Lucid.img_ [Lucid.class_ "col-span-1 inline", Lucid.src_ src]

renderBadges :: Lucid.Html ()
renderBadges =
  Lucid.div_ [Lucid.class_ "mx-auto max-w-screen-xl"] $ do
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
