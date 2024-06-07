{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (app) where

import Control.Monad.Reader (runReaderT)
import Servant (
  Proxy (Proxy),
  Raw,
  serveDirectoryFileServer,
  (:<|>) ((:<|>)),
 )
import Servant.Server

import Api.User (UserAPI, userApi, userServer)
import Api.Base (BaseAPI, baseApi, baseServer)
import Config (AppT (..), Config (..))

{- | This functions tells Servant how to run the 'App' monad with our
'server' function.
-}
appToUserServer :: Config -> Server UserAPI
appToUserServer cfg = hoistServer userApi (convertApp cfg) userServer


appToBaseServer :: Config -> Server BaseAPI
appToBaseServer cfg = hoistServer baseApi (convertApp cfg) baseServer

{- | This function converts our @'AppT' m@ monad into the @ExceptT ServantErr
m@ monad that Servant's 'enter' function needs in order to run the
application.
-}
convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

{- | Since we also want to provide a minimal front end, we need to give
Servant a way to serve a directory with HTML and CSS. This
function creates a WAI application that just serves the files out of the
given directory.
-}
files :: Server Raw
files = serveDirectoryFileServer "static"

{- | Just like a normal API type, we can use the ':<|>' combinator to unify
two different APIs and applications. This is a powerful tool for code
reuse and abstraction! We need to put the 'Raw' endpoint last, since it
always succeeds.
-}
type AppAPI = UserAPI :<|> BaseAPI :<|> Raw

appApi :: Proxy AppAPI
appApi = Proxy

{- | Finally, this function takes a configuration and runs our 'UserAPI'
alongside the 'Raw' endpoint that serves all of our files.
-}
app :: Config -> Application
app cfg = do
  let userServerCfg = appToUserServer cfg
      appServerCfg = appToBaseServer cfg
  serve appApi (userServerCfg :<|> appServerCfg :<|> files)
