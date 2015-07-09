{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Controllers.Sessions (spec) where

import           Control.Applicative
import           Network.HTTP.Types
import           Network.Wai.Test    (SResponse(simpleHeaders))
import           Test.Hspec          (describe, it)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           Util

spec =
  describe "Sessions" $ do
    it "creating new session with invalide input should fail" $ do
      post "/sessions" [json|{invalid:"input"}|] `shouldRespondWith` 400

    it "creating new session with valid credential should succeed" $ do
      createTestUser `shouldRespondWith` 201
      loginTestUser `shouldRespondWith` 201

    it "creating new session with invalid credential should fail" $ do
      loginTestUser `shouldRespondWith` 400

    it "creating new session twice should succeed" $ do
      createTestUser `shouldRespondWith` 201
      loginTestUser `shouldRespondWith` 201
      loginTestUser `shouldRespondWith` 201

    it "getting current session before having a session should fail" $ do
      get "/sessions" `shouldRespondWith` 404

    it "getting current session after having a session should succeed" $ do
      createTestUser
      (Just cookie) <- getCookie <$> loginTestUser
      request methodGet "/sessions" [("Cookie", cookie)] "" `shouldRespondWith` [json|{name:"ecky"}|]

    it "getting current session after removing current session should fail" $ do
      createTestUser `shouldRespondWith` 201
      loginTestUser `shouldRespondWith` 201
      delete "/sessions" `shouldRespondWith` 200
      get "/sessions" `shouldRespondWith` 404

    it "removing current session without having a session should succeed" $ do
      delete "/sessions" `shouldRespondWith` 200
