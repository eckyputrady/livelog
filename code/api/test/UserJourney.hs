{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module UserJourney (spec) where

import           Test.Hspec (describe, it)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import Network.HTTP.Types
import Network.Wai.Test (SResponse(simpleHeaders))
import Util


spec = 
  it "UserJourney" $ pending
    -- user registers to the application
    -- POST /users {name:"user",pass:"password"}

    -- user logins to the application
    -- > POST /sessions {name:"user",pass:"password"}

    -- user logs his current activity
    -- > POST /logs {message:"writing test"}

    -- after a while, the user do another activity
    -- > POST /logs {message:"replying reddit thread"}
    
    -- the last activity has finished, now he wants to tidy up the logs
    -- > POST /logs {message:"Tidying up logs"}

    -- he created some tags to categorize the activity
    -- > POST /tags {name:"haskell"}
    -- > POST /tags {name:"livelog"}

    -- he tags previous activities with tags he just created
    -- > POST /taglog {tagId:1, logId:1}
    -- > POST /taglog {tagId:1, logId:2}
    -- > POST /taglog {tagId:2, logId:3}

    -- he wants to know what activities he has done today
    -- > GET /logs

    -- he wants to know what activities he has done today that are about "haskell"
    -- > GET /tags/1/logs

    -- having done tidying the logs, he set his current activity as "rest"
    -- > POST /logs {message:"Rest"}

    -- user logout from the system
    -- > DELETE /sessions
