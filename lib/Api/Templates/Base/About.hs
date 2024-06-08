module Api.Templates.Base.About where

import Api.Templates.Base.Home (renderBadges)
import Lucid

renderAbout :: Html ()
renderAbout =
  div_ [class_ "mt-4"] $ do
    div_ [class_ "max-w-screen-xl mx-auto"] $ do
      h1_ [class_ "text-xl font-bold text-gray-900"] "About"
      p_ [class_ "text-gray-600 mt-1"] $ do
        "hastl is a modern Haskell web application using Htmx, Alpine.js, Servant,"
        "Tailwind-css and Lucid. It is licensed under MIT and is entirely free and open source."
        br_ []
        br_ []
        "This is a simple about page, showing one of the features of the hastl template, routing."
        "Hastl allows you to split your application into multiple strongly typed Servant API's and combine different endpoints that return either JSON, raw HTML or HTMX-enabled hypermedia."
        br_ []
        br_ []
        "Additionally, hastl provides tooling to auto-reload your server on changes and recompile your tailwindcss stylesheets automatically, giving you a fast feedback loop. "
        "Check out "
        a_ [href_ "https://github.com/eldr-io/hastl", target_ "_blank"] "the hastl docs"
        " for more information."
        br_ []
        br_ []
        b_ "hastl was built on top of the awesome servant-persistant example by Matt Parsons, so be sure to "
        a_ [href_ "https://github.com/parsonsmatt/servant-persistent", target_ "_blank"] "check it out"
        "!"
        div_ [class_ "mt-5"] $ do
          h2_ [class_ "text-md font-bold text-gray-900"] "Built with:"
          renderBadges
