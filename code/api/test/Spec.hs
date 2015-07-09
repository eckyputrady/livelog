import           System.Directory
import           Test.Hspec
import           Test.Hspec.Wai

import qualified Controllers.LogTag   as CLogTag
import qualified Controllers.Logs     as CLogs
import qualified Controllers.Sessions as CSessions
import qualified Controllers.Tags     as CTags
import qualified Controllers.Users    as CUsers
import           Lib                  (clearDB, getConfig, runApp)
import qualified UserJourney

main :: IO ()
main = do
  getCurrentDirectory >>= putStrLn
  c <- getConfig
  hspec $ spec c

spec c =
  after_ (clearDB c) $ with (runApp c) $ do
    CUsers.spec
    CSessions.spec
    CLogs.spec
    CTags.spec
    CLogTag.spec
    UserJourney.spec
