{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Controllers.Users (spec) where

import           Test.Hspec          (describe, it)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

spec =
  describe "Users" $ do
    it "creating new user should succeed" $
      post "/users" [json|{name:"ecky", pass:"putrady"}|] `shouldRespondWith` 201
    it "creating new user with the same username should fail" $ do
      post "/users" [json|{name:"ecky", pass:"putrady"}|] `shouldRespondWith` 201
      post "/users" [json|{name:"ecky", pass:"putrady"}|] `shouldRespondWith` 400
      post "/users" [json|{name:"putrady", pass:"ecky"}|] `shouldRespondWith` 201
      post "/users" [json|{name:"putrady", pass:"ecky"}|] `shouldRespondWith` 400
