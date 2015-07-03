{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Controllers.Tags (spec) where

import           Test.Hspec (describe, it)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

spec = 
  describe "Tags" $ do
    describe "Unauthorized users" $ do
      it "should fail to query" $ pending
      it "should fail to get" $ pending
      it "should fail to post" $ pending
      it "should fail to put" $ pending
      it "should fail to delete" $ pending
    describe "Authorized users" $ do
      it "should return empty if user has not input anything" $ pending
      it "should return the tags the user has created" $ pending
      it "should be able to update the tags the user has created" $ pending
      it "should fail to update the tags the user has NOT created" $ pending
      it "should be able to delete tag the user has created" $ pending
      it "should fail to delete the user has NOT created" $ pending