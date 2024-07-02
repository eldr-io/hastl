module Api.UserSpec where

import Api.Templates.User.User (renderUser)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Models (User (User), userCreatedAt, userEmail, userName)
import Test.Hspec (Spec, before, context, describe, it, shouldBe)

import Database.Persist.Postgresql (
  Entity (..),
  Key (..),
 )

spec :: Spec
spec = before
  ( do
      time <- getCurrentTime
      pure time
  )
  $ do
    describe "templates" $ do
      context "when rendering a user" $ do
        it "renders a user" $
          ( \currentTime -> do
              let user =
                    User
                      { userName = "user"
                      , userEmail = "hi@mom.com"
                      , userCreatedAt = currentTime
                      }
              -- let entity = Entity (Key user) user
              userName user `shouldBe` "user"
          )
