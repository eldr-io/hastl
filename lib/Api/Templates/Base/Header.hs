module Api.Templates.Base.Header where

import Api.Templates.Helpers.Alpine
import Api.Templates.Helpers.Htmx
import Api.Templates.Helpers.Icons (iconChevronDown_, iconGithub_)
import Data.Text (Text)
import Lucid

renderHeader :: Html () -> Html ()
renderHeader title = do
  head_ $ do
    meta_ [charset_ "utf-8"]
    title_ title
    link_ [rel_ "stylesheet", type_ "text/css", href_ "./css/style.css"]

renderBanner :: Html ()
renderBanner = do
  div_
    [ class_ "relative isolate flex items-center gap-x-6 overflow-hidden bg-gray-50 px-6 py-1 sm:px-3.5 sm:before:flex-1"
    ]
    $ do
      div_
        [ class_ "absolute left-[max(-7rem,calc(50%-52rem))] top-1/2 -z-10 -translate-y-1/2 transform-gpu blur-2xl"
        , ariaHidden_ "true"
        ]
        $ do
          div_
            [ class_ "aspect-[577/310] w-[36.0625rem] bg-gradient-to-r from-[#ff80b5] to-[#9089fc] opacity-30"
            , style_ "clip-path: polygon(74.8% 41.9%, 97.2% 73.2%, 100% 34.9%, 92.5% 0.4%, 87.5% 0%, 75% 28.6%, 58.5% 54.6%, 50.1% 56.8%, 46.9% 44%, 48.3% 17.4%, 24.7% 53.9%, 0% 27.9%, 11.9% 74.2%, 24.9% 54.1%, 68.6% 100%, 74.8% 41.9%)"
            ]
            mempty
      div_ [class_ "absolute left-[max(45rem,calc(50%+8rem))] top-1/2 -z-10 -translate-y-1/2 transform-gpu blur-2xl", ariaHidden_ "true"] $ do
        div_
          [ class_ "aspect-[577/310] w-[36.0625rem] bg-gradient-to-r from-[#ff80b5] to-[#9089fc] opacity-30"
          , style_ "clip-path: polygon(74.8% 41.9%, 97.2% 73.2%, 100% 34.9%, 92.5% 0.4%, 87.5% 0%, 75% 28.6%, 58.5% 54.6%, 50.1% 56.8%, 46.9% 44%, 48.3% 17.4%, 24.7% 53.9%, 0% 27.9%, 11.9% 74.2%, 24.9% 54.1%, 68.6% 100%, 74.8% 41.9%)"
          ]
          mempty
      div_
        [ class_ "flex flex-wrap items-center gap-x-4 gap-y-2"
        ]
        $ do
          p_
            [ class_ "text-sm leading-6 text-gray-900"
            ]
            $ do
              strong_ [class_ "font-semibold"] "λ hastl is built with haskell and is entirely free and open source"
              svg_
                [ viewBox_ "0 0 2 2"
                , class_ "mx-2 inline h-0.5 w-0.5 fill-current"
                , ariaHidden_ "true"
                ]
                $ do
                  circle_ [cx_ "1", cy_ "1", r_ "1"] mempty
              "Licensed under MIT"
          a_
            [ href_ "https://github.com/eldr-io/hastl"
            , target_ "_blank"
            , class_ "flex-none rounded-full bg-gray-900 px-3.5 py-1 text-sm font-semibold text-white shadow-sm hover:bg-gray-700 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-gray-900"
            ]
            $ do
              "Get started "
              span_ [ariaHidden_ "true"] "→"
      div_ [class_ "flex flex-1 justify-end"] $ do
        button_ [type_ "button", class_ "-m-3 p-3 focus-visible:outline-offset-[-4px]"] $ do
          span_ [class_ "sr-only"] "Dismiss"
          svg_ [class_ "h-5 w-5 text-gray-900", viewBox_ "0 0 20 20", fill_ "currentColor", ariaHidden_ "true"] $ do
            path_ [d_ "M6.28 5.22a.75.75 0 00-1.06 1.06L8.94 10l-3.72 3.72a.75.75 0 101.06 1.06L10 11.06l3.72 3.72a.75.75 0 101.06-1.06L11.06 10l3.72-3.72a.75.75 0 00-1.06-1.06L10 8.94 6.28 5.22z"] mempty

renderDropdownBtn :: Html ()
renderDropdownBtn = do
  div_
    [ xData_ "{ open: false }"
    , class_ "inline-flex relative"
    ]
    $ do
      button_
        [ xRef_ "button"
        , xOn_ "click" "open = ! open"
        , class_ "px-4 ps-10 pe-10 py-2 border border-gray-200 rounded-lg shadow-sm bg-white"
        ]
        $ do
          span_ [class_ "select-none absolute inset-y-0 right-0 flex items-center cursor-pointer pr-3"] $ do
            iconChevronDown_
          span_ [class_ "select-none absolute inset-y-0 left-0 flex items-center cursor-pointer pl-3"] $ do
            iconGithub_
          "Get the Code"
      div_
        [ xCloak_
        , xOn_ "click.away" "open = false"
        , xShow_ "open"
        , class_ "w-full absolute top-12 left-0 p-2 bg-white border border-gray-200 rounded-lg shadow"
        ]
        $ do
          div_
            [ xOn_ "click" "window.open('https://github.com/eldr-io/hastl', '_blank').focus();"
            , class_ "px-2 py-1 cursor-pointer hover:bg-sky-100 rounded-lg"
            ]
            "Github"
          div_
            [ xOn_ "click" "window.open('https://github.com/eldr-io/hastl/blob/main/README.md', '_blank').focus();"
            , class_ "px-2 py-1 cursor-pointer hover:bg-sky-100 rounded-lg"
            ]
            "Documentation"

renderNavigation :: Text -> Html ()
renderNavigation activeItem = do
  nav_ [class_ "bg-white border-gray-200 py-2.5 dark:bg-gray-900"] $ do
    div_ [class_ "flex flex-wrap items-center justify-between max-w-screen-xl px-4 mx-auto"] $ do
      a_ [href_ "/", class_ "flex items-center"] $ do
        img_ [src_ "./img/lambda.svg", class_ "w-8 h-8 mr-2", alt_ "hastl"]
        span_ [class_ "self-center text-xl font-semibold whitespace-nowrap dark:text-white"] "hastl"
      div_ [class_ "flex items-center lg:order-2"] $ do
        renderDropdownBtn
      div_ [class_ "items-center justify-between w-full lg:flex lg:w-auto lg:order-1", id_ "mobile-menu-2"] $ do
        ul_ [class_ "flex flex-col mt-4 font-medium lg:flex-row lg:space-x-8 lg:mt-0"] $ do
          mapM_ renderNavigationItem [("/", "Home", activeItem == "Home"), ("/about", "About", activeItem == "About")]

renderNavigationItem :: (Text, Text, Bool) -> Html ()
renderNavigationItem (href, text, False) = do
  li_ $ a_ [href_ href, class_ "block py-2 pl-3 pr-4 text-gray-700 border-b border-gray-100 hover:bg-gray-50 lg:hover:bg-transparent lg:border-0 lg:hover:text-purple-700 lg:p-0 dark:text-gray-400 lg:dark:hover:text-white dark:hover:bg-gray-700 dark:hover:text-white lg:dark:hover:bg-transparent dark:border-gray-700"] (toHtml text)
renderNavigationItem (href, text, True) = do
  li_ $ a_ [href_ href, class_ "block py-2 pl-3 pr-4 text-white bg-purple-700 rounded lg:bg-transparent lg:text-purple-700 lg:p-0 dark:text-white"] (toHtml text)
