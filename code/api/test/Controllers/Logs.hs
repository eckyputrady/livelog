{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Controllers.Logs (spec) where

import           Control.Applicative
import           Data.ByteString.Lazy (toStrict)
import           Data.Text            (isInfixOf)
import           Data.Text.Encoding   (decodeUtf8)
import           Network.HTTP.Types
import           Network.Wai.Test     (SResponse(simpleBody))
import           Test.Hspec           (describe, it, shouldSatisfy)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           Util

spec =
  describe "Logs" $ do

    describe "Unauthorized users" $ do

      it "should fail to query" $ do
        setupInitialData
        get "/logs" `shouldRespondWith` 404

      it "should fail to get" $ do
        setupInitialData
        get "/logs/1" `shouldRespondWith` 404

      it "should fail to post" $ do
        setupInitialData
        post "/logs/" [json|{message:"should fail"}|] `shouldRespondWith` 404

      it "should fail to put" $ do
        setupInitialData
        put "/logs/1" [json|{message:"should fail"}|] `shouldRespondWith` 404

      it "should fail to delete" $  do
        setupInitialData
        delete "/logs/1" `shouldRespondWith` 404

    describe "Authorized users" $ do
      it "should return empty if user has not input anything" $ do
        setupInitialData
        (Just c) <- getCookie <$> loginTestUser
        request methodGet "/logs" [("Cookie", c)] "" `shouldRespondWith` "[]"

      it "should return the logs the user has created" $ do
        setupInitialData
        (Just c) <- getCookie <$> loginTestUser
        request methodPost "/logs" [("Cookie", c)] [json|{message:"test 3"}|] `shouldRespondWith` 201
        response <- request methodGet "/logs" [("Cookie", c)] ""
        let body = decodeUtf8 . toStrict . simpleBody $ response
        liftIO $ body `shouldSatisfy` (isInfixOf ":\"test 3\"")

      it "should not be able to access logs that is not his" $ do
        setupInitialData
        (Just c) <- getCookie <$> loginTestUser
        request methodGet "/logs/1" [("Cookie", c)] "" `shouldRespondWith` 404

      it "should be able to update the logs the user has created" $ do
        setupInitialData
        (Just c) <- getCookie <$> loginTestUser
        request methodPost "/logs" [("Cookie", c)] [json|{message:"test 3"}|] `shouldRespondWith` 201
        request methodPut "/logs/3" [("Cookie", c)] [json|{message:"test-test 3"}|] `shouldRespondWith` 200
        response <- request methodGet "/logs/3" [("Cookie", c)] ""
        let body = decodeUtf8 . toStrict . simpleBody $ response
        liftIO $ body `shouldSatisfy` (isInfixOf "test-test 3")

      it "should fail to update logs the user has NOT created" $ do
        setupInitialData
        (Just c) <- getCookie <$> loginTestUser
        request methodPut "/logs/1" [("Cookie", c)] [json|{message:"test-test 3"}|] `shouldRespondWith` 404

      it "should be able to delete log the user has created" $ do
        setupInitialData
        (Just c) <- getCookie <$> loginTestUser
        request methodPost "/logs" [("Cookie", c)] [json|{message:"test 3"}|] `shouldRespondWith` 201
        request methodDelete "/logs/3" [("Cookie", c)] "" `shouldRespondWith` 200

      it "should fail to delete the log user has NOT created" $ do
        setupInitialData
        (Just c) <- getCookie <$> loginTestUser
        request methodDelete "/logs/1" [("Cookie", c)] "" `shouldRespondWith` 404
        request methodDelete "/logs/3" [("Cookie", c)] "" `shouldRespondWith` 404

setupInitialData = do
  createTestUser
  createUser "dummy"
  (Just c) <- getCookie <$> loginUser "dummy"
  request methodPost "/logs" [("Cookie", c)] [json|{message:"test 1"}|] `shouldRespondWith` 201
  request methodPost "/logs" [("Cookie", c)] [json|{message:"test 2"}|] `shouldRespondWith` 201
