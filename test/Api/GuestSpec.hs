module Api.GuestSpec where

import Data.Time (getCurrentTime)
import Models (Guest (Guest), guestCreatedAt, guestEmail, guestName)
import Test.Hspec (Spec, before, context, describe, it, shouldBe)

spec :: Spec
spec = before
  ( do
      time <- getCurrentTime
      pure time
  )
  $ do
    describe "templates" $ do
      context "when rendering a guest" $ do
        it "renders a guest" $
          ( \currentTime -> do
              let guest =
                    Guest
                      { guestName = "bobby"
                      , guestEmail = "hi@mom.com"
                      , guestCreatedAt = currentTime
                      }
              -- let entity = Entity (Key user) user
              guestName guest `shouldBe` "bobby"
          )
