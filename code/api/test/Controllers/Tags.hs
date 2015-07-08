{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Controllers.Tags (spec) where

import           Test.Hspec (describe, it)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import Util
import Network.HTTP.Types
import Control.Applicative

spec = 
  describe "Tags" $ do

    describe "Unauthorized users" $ do

      it "should fail to query" $ do
        setupInitialData
        get "/tags" `shouldRespondWith` 404

      it "should fail to get" $ do
        setupInitialData
        get "/tags/1" `shouldRespondWith` 404

      it "should fail to post" $ do
        setupInitialData
        post "/tags/" [json|{name:"should fail"}|] `shouldRespondWith` 404

      it "should fail to put" $ do
        setupInitialData
        put "/tags/1" [json|{name:"should fail"}|] `shouldRespondWith` 404

      it "should fail to delete" $  do
        setupInitialData
        delete "/tags/1" `shouldRespondWith` 404

    describe "Authorized users" $ do
      it "should return empty if user has not input anything" $ do
        setupInitialData
        (Just c) <- getCookie <$> loginTestUser
        request methodGet "/tags" [("Cookie", c)] "" `shouldRespondWith` "[]"

      it "should return the items the user has created" $ do
        setupInitialData
        (Just c) <- getCookie <$> loginTestUser
        request methodPost "/tags" [("Cookie", c)] [json|{name:"test 3"}|] `shouldRespondWith` 201
        request methodGet "/tags" [("Cookie", c)] "" `shouldRespondWith` [json|[{name:"test 3",id:3,userId:1}]|]

      it "should not be able to access item that is not his" $ do
        setupInitialData
        (Just c) <- getCookie <$> loginTestUser
        request methodGet "/tags/1" [("Cookie", c)] "" `shouldRespondWith` 404

      it "should be able to update the items the user has created" $ do
        setupInitialData
        (Just c) <- getCookie <$> loginTestUser
        request methodPost "/tags" [("Cookie", c)] [json|{name:"test 3"}|] `shouldRespondWith` 201
        request methodPut "/tags/3" [("Cookie", c)] [json|{name:"test-test 3"}|] `shouldRespondWith` 200
        request methodGet "/tags/3" [("Cookie", c)] "" `shouldRespondWith` [json|{name:"test-test 3",id:3,userId:1}|]

      it "should fail to update items the user has NOT created" $ do
        setupInitialData
        (Just c) <- getCookie <$> loginTestUser
        request methodPut "/tags/1" [("Cookie", c)] [json|{name:"test-test 3"}|] `shouldRespondWith` 404

      it "should be able to delete item the user has created" $ do
        setupInitialData
        (Just c) <- getCookie <$> loginTestUser
        request methodPost "/tags" [("Cookie", c)] [json|{name:"test 3"}|] `shouldRespondWith` 201
        request methodDelete "/tags/3" [("Cookie", c)] "" `shouldRespondWith` 200
        request methodGet "/tags/3" [("Cookie", c)] "" `shouldRespondWith` 404

      it "should fail to delete the item user has NOT created" $ do
        setupInitialData
        (Just c) <- getCookie <$> loginTestUser
        request methodDelete "/tags/1" [("Cookie", c)] "" `shouldRespondWith` 404
        request methodDelete "/tags/3" [("Cookie", c)] "" `shouldRespondWith` 404

setupInitialData = do
  createTestUser
  createUser "dummy"
  (Just c) <- getCookie <$> loginUser "dummy"
  request methodPost "/tags" [("Cookie", c)] [json|{name:"test 1"}|] `shouldRespondWith` 201
  request methodPost "/tags" [("Cookie", c)] [json|{name:"test 2"}|] `shouldRespondWith` 201