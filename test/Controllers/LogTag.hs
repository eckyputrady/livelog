{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Controllers.LogTag (spec) where

import           Test.Hspec (describe, it)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import Util
import Network.HTTP.Types

spec = 
  describe "LogTag" $ do

    describe "Unauthorized users" $ do

      it "should fail to query" $ do
        setupInitialData
        get "/tags/1/logs" `shouldRespondWith` 404
        get "/logs/1/tags" `shouldRespondWith` 404

      it "should fail to post" $ do
        setupInitialData
        post "/taglog" [json|{logId:1,tagId:1}|] `shouldRespondWith` 404

      it "should fail to delete" $  do
        setupInitialData
        delete "/tags/1/logs/1" `shouldRespondWith` 404
        delete "/logs/1/tags/1" `shouldRespondWith` 404

    describe "Authorized users" $ do
      it "should fail to tag non-existent log" $ pending
      it "should fail to tag a log with non-existent tag" $ pending
      it "should success to tag a log with both tag & log being exists" $ pending

      it "should list tags from a log properly" $ pending
      it "should list logs from a tag properly" $ pending

setupInitialData = do
  createTestUser
  createUser "dummy"
  (Just c) <- loginUser "dummy" >>= return . getCookie
  request methodPost "/tags" [("Cookie", c)] [json|{name:"test 1"}|] `shouldRespondWith` 201
  request methodPost "/tags" [("Cookie", c)] [json|{name:"test 2"}|] `shouldRespondWith` 201
  request methodPost "/logs" [("Cookie", c)] [json|{message:"test 1"}|] `shouldRespondWith` 201
  request methodPost "/taglog" [("Cookie", c)] [json|{logId:1,tagId:1}|] `shouldRespondWith` 201
  request methodPost "/taglog" [("Cookie", c)] [json|{logId:1,tagId:2}|] `shouldRespondWith` 201