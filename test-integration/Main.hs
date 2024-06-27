module Main where

import Test.Hspec
import TestHttpSpec (specHttp)

main :: IO ()
main = hspec specHttp
