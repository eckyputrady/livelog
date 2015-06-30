{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Lib (runApp, getConfig)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

main :: IO ()
main = do
  c <- getConfig
  hspec $ spec (runApp c)

spec app = with app $ do
    describe "Users" $ do
      it "Should create new user" $ do
        get "/" `shouldRespondWith` 200
