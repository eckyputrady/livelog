{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Controllers.Users (spec) where

import           Test.Hspec (describe, it)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

spec = 
  describe "Users" $ do
    it "creating new user should succeed" $ pending
    it "creating new user with the same username should fail" $ pending