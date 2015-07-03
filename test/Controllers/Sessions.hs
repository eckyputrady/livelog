{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Controllers.Sessions (spec) where

import           Test.Hspec (describe, it)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

spec = 
  describe "Sessions" $ do
    it "creating new session with valid credential should succeed" $ pending
    it "creating new session with invalid credential should fail" $ pending
    it "getting current session before having a session should fail" $ pending
    it "getting current session after having a session should succeed" $ pending
    it "getting current session after removing current session should fail" $ pending
    it "removing current session without having a session should succeed" $ pending