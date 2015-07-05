{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Util where

import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import Network.HTTP.Types
import Network.Wai.Test (SResponse(simpleHeaders))

createTestUser = post "/users" [json|{name:"ecky", pass:"putrady"}|]
loginTestUser = post "/sessions" [json|{name:"ecky", pass:"putrady"}|]
getCookie = lookup "Set-Cookie" . simpleHeaders