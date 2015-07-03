import Lib (runApp, getConfig)

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
  hspec $ spec (runApp c)

spec app = with app $ do
      CUsers.spec
      CSessions.spec
      CLogs.spec
      CTags.spec
      CLogTag.spec
