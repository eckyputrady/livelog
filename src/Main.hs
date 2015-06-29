module Main where

import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.MySQL (createMySQLPool, defaultConnectInfo, connectDatabase, connectPassword)
import qualified Data.Vault.Lazy as Vault
import App
import Types

main :: IO ()
main = do 
  c <- getConfig
  runApp c ioRunner

getConfig = do
  p   <- runStdoutLoggingT $ createMySQLPool connInfo 10
  vk  <- Vault.newKey
  return $ Config { pool = p, vaultKey = vk }
  where connInfo = defaultConnectInfo { connectDatabase = "livelog", connectPassword = "" }
