import Lib (runApp, getConfig, clearDB)

import           Test.Hspec
import           Test.Hspec.Wai

import qualified Controllers.Users as CUsers
import qualified Controllers.Sessions as CSessions
import qualified Controllers.Logs as CLogs
import qualified Controllers.Tags as CTags
import qualified Controllers.LogTag as CLogTag

main :: IO ()
main = do
  c <- getConfig
  hspec $ spec c

spec c = 
	after_ (clearDB c) $ with (runApp c) $ do
    CUsers.spec
    CSessions.spec
    -- CLogs.spec
    -- CTags.spec
    -- CLogTag.spec
