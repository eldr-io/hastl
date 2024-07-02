module TestHttpSpec (specHttp) where

import Test.Hspec 
import TestContainers.Docker qualified as TC
import TestContainers.Hspec qualified as TC
import Network.HTTP.Simple


containers :: TC.TestContainer Int
containers = do
  image <- TC.build (TC.fromBuildContext "./" (Just "./test-integration/test-server/Dockerfile"))
  tc <-
    TC.run
      ( TC.containerRequest image
          TC.& TC.setFixedName "hastl-integration-test"
          TC.& TC.withFollowLogs TC.consoleLogConsumer 
          TC.& TC.setExpose [8000]
          TC.& TC.setWaitingFor (TC.waitUntilMappedPortReachable 8000)
      )
  pure (TC.containerPort tc 8000)

specHttp :: Spec
specHttp =
  around (TC.withContainers containers) $
    describe "integration test using a mock API" $ do
      it "can successfully HTTP GET the root of the test server" $ \port ->
        do
          request <- parseRequest "GET http://localhost/"
          let request' = setRequestPort port request
          response <- httpBS request'
          getResponseStatusCode response `shouldBe` 200




