{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Util where

import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import Network.HTTP.Types
import Network.Wai.Test (SResponse(simpleHeaders))

createTestUser = createUser "ecky"
loginTestUser = loginUser "ecky"
getCookie = lookup "Set-Cookie" . simpleHeaders

createUser :: String -> WaiSession SResponse
createUser name = post "/users" [json|{name:#{name}, pass:"putrady"}|]
loginUser :: String -> WaiSession SResponse
loginUser name = post "/sessions" [json|{name:#{name}, pass:"putrady"}|]