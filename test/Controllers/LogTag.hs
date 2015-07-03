{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Controllers.LogTag (spec) where

import           Test.Hspec (describe, it)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

spec = 
  describe "LogTag" $ do
    describe "Unauthorized users" $ do
      it "should fail to query" $ pending
      it "should fail to get" $ pending
      it "should fail to post" $ pending
      it "should fail to put" $ pending
      it "should fail to delete" $ pending
    describe "Authorized users" $ do
      it "should fail to tag non-existent log" $ pending
      it "should fail to tag a log with non-existent tag" $ pending
      it "should success to tag a log with both tag & log being exists" $ pending

      it "should list tags from a log properly" $ pending
      it "should list logs from a tag properly" $ pending