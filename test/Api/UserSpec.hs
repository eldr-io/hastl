module Api.User.UserSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import hastl.Api.Templates.User (renderUser)

spec :: Spec
spec = do
  describe "renderUser" $ do
    it "renders a user" $ do
      renderUser `shouldBe` "<div>user</div>"
