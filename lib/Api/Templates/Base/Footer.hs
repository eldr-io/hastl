{-# LANGUAGE OverloadedStrings #-}

module Api.Templates.Base.Footer where

import Lucid (Html, class_, div_, p_)

renderFooter :: Html ()
renderFooter = div_ [class_ "footer"] (p_ "This is the footer")
