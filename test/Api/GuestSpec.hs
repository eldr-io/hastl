module Api.GuestSpec where

import Api.Guest (CreateGuestPayload(CreateGuestPayload), ValidationError(..), validateGuest)
import Data.Time (getCurrentTime)
import Models (Guest (Guest), guestCreatedAt, guestEmail, guestName)
import Test.Hspec (Spec, before, context, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "guest validation" $ do
    context "when validating guest payload" $ do
      it "accepts valid guest data" $ do
        let payload = CreateGuestPayload "John Doe" "john@example.com"
        validateGuest payload `shouldBe` Right payload
      
      it "rejects empty name" $ do
        let payload = CreateGuestPayload "" "john@example.com"
        validateGuest payload `shouldBe` Left [EmptyName]
      
      it "rejects whitespace-only name" $ do
        let payload = CreateGuestPayload "   " "john@example.com"
        validateGuest payload `shouldBe` Left [EmptyName]
      
      it "rejects empty email" $ do
        let payload = CreateGuestPayload "John Doe" ""
        validateGuest payload `shouldBe` Left [EmptyEmail]
      
      it "rejects invalid email format" $ do
        let payload = CreateGuestPayload "John Doe" "not-an-email"
        validateGuest payload `shouldBe` Left [InvalidEmail]
      
      it "returns multiple errors for multiple invalid fields" $ do
        let payload = CreateGuestPayload "" "not-an-email"
        validateGuest payload `shouldBe` Left [EmptyName, InvalidEmail]

  before
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
