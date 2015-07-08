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

    -- user logins to the application

    -- user logs his current activity

    -- after a while, the user do another activity

    -- the last activity has finished, now he wants to tidy up the logs

    -- he created some tags to categorize the activity

    -- he tags previous activities with tags he just created

    -- having done tidying the logs, he set his current activity as "rest"

    -- he wants to know what activities he has done today

    -- he wants to know what activities he has done today that are about "haskell"

    -- user logout from the system