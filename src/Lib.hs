module Lib (main, getConfig, runAppIO, runApp, clearDB) where

import Control.Monad.Logger (runStdoutLoggingT, runNoLoggingT)
import Database.Persist.MySQL (createMySQLPool, runSqlPool, defaultConnectInfo, connectDatabase, connectPassword)
import qualified Data.Vault.Lazy as Vault
import App
import Types
import Model

main :: IO ()
main = do 
  c <- getConfig
  runAppIO c

getConfig = do
  p   <- runNoLoggingT $ createMySQLPool connInfo 10
  vk  <- Vault.newKey
  return $ Config { pool = p, vaultKey = vk }
  where connInfo = defaultConnectInfo { connectDatabase = "livelog", connectPassword = "" }

clearDB :: Config -> IO ()
clearDB c = runSqlPool clearModels (pool c)