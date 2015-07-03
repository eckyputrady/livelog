{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Controllers.Logs (spec) where

import           Test.Hspec (describe, it)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

spec = 
  describe "Logs" $ do
    describe "Unauthorized users" $ do
      it "should fail to query" $ pending
      it "should fail to get" $ pending
      it "should fail to post" $ pending
      it "should fail to put" $ pending
      it "should fail to delete" $ pending
    describe "Authorized users" $ do
      it "should return empty if user has not input anything" $ pending
      it "should return the logs the user has created" $ pending
      it "should be able to update the logs the user has created" $ pending
      it "should fail to update logs the user has NOT created" $ pending
      it "should be able to delete log the user has created" $ pending
      it "should fail to delete the log user has NOT created" $ pending