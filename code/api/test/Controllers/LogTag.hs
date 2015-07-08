{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Controllers.LogTag (spec) where

import           Test.Hspec (describe, it)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import Util
import Network.HTTP.Types
import Control.Applicative

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

      it "should fail to tag non-existent log" $ do
        c <- setupInitialData
        request methodPost "/taglog" [("Cookie", c)] [json|{logId:10,tagId:1}|] `shouldRespondWith` 400

      it "should OK to tag the same log more than once" $ do
        c <- setupInitialData
        request methodPost "/taglog" [("Cookie", c)] [json|{logId:1,tagId:1}|] `shouldRespondWith` 201

      it "should fail to tag a log with non-existent tag" $ do
        c <- setupInitialData
        request methodPost "/taglog" [("Cookie", c)] [json|{logId:1,tagId:10}|] `shouldRespondWith` 400

      it "should success to tag a log with both tag & log being exists" $ do
        c <- setupInitialData
        request methodPost "/logs" [("Cookie", c)] [json|{message:"test 2"}|] `shouldRespondWith` 201
        request methodPost "/taglog" [("Cookie", c)] [json|{logId:2,tagId:2}|] `shouldRespondWith` 201

      it "should not be able to tag something that not his" $ do
        setupInitialData
        (Just c) <- getCookie <$> loginTestUser
        request methodPost "/taglog" [("Cookie", c)] [json|{logId:1,tagId:1}|] `shouldRespondWith` 400

      it "should be able to untag" $ do
        c <- setupInitialData
        request methodDelete "/logs/1/tags/1" [("Cookie", c)] "" `shouldRespondWith` 200
        request methodDelete "/tags/2/logs/1" [("Cookie", c)] "" `shouldRespondWith` 200
        request methodGet "/logs/1/tags" [("Cookie", c)] "" `shouldRespondWith` "[]"

      it "should not be able to untag items that not his" $ do
        setupInitialData
        (Just c) <- getCookie <$> loginTestUser
        request methodDelete "/logs/1/tags/1" [("Cookie", c)] "" `shouldRespondWith` 404

      it "should list tags from a log properly" $ do
        c <- setupInitialData
        request methodGet "/logs/1/tags" [("Cookie", c)] "" `shouldRespondWith` [json|[{id:1,userId:2,name:"test 1"},{id:2,userId:2,name:"test 2"}]|]

      it "should list logs from a tag properly" $ 
        pendingWith "unable to find a better method to check for messages (it has 'current time' value)"

setupInitialData = do
  createTestUser
  createUser "dummy"
  (Just c) <- getCookie <$> loginUser "dummy"
  request methodPost "/tags" [("Cookie", c)] [json|{name:"test 1"}|] `shouldRespondWith` 201
  request methodPost "/tags" [("Cookie", c)] [json|{name:"test 2"}|] `shouldRespondWith` 201
  request methodPost "/logs" [("Cookie", c)] [json|{message:"test 1"}|] `shouldRespondWith` 201
  request methodPost "/taglog" [("Cookie", c)] [json|{logId:1,tagId:1}|] `shouldRespondWith` 201
  request methodPost "/taglog" [("Cookie", c)] [json|{logId:1,tagId:2}|] `shouldRespondWith` 201
  return c