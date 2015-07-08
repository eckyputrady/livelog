{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lib (main, getConfig, runAppIO, runApp, clearDB) where

import qualified Data.ByteString.Lazy as B
import Control.Monad.Logger (runStdoutLoggingT, runNoLoggingT)
import Database.Persist.MySQL (createMySQLPool, runSqlPool, defaultConnectInfo, connectDatabase, connectPassword, connectUser, connectHost)
import qualified Data.Vault.Lazy as Vault
import App
import Types
import Model
import qualified Data.Aeson as Aeson
import System.Environment
import Control.Applicative
import Control.Exception (try, SomeException(..), catch)
import Control.Concurrent (threadDelay)
import Data.Either
import System.IO.Error

main :: IO ()
main = do
  c <- getConfig
  runAppIO c `catch` \(e :: SomeException) -> do
    print e
    putStrLn "Failed to start, waiting & retry"
    threadDelay $ 5 * 1000 * 1000
    main

getConfig = do
  vk <- Vault.newKey
  rawCfg <- readRawConfig
  p <- runNoLoggingT $ createMySQLPool (connInfo rawCfg) 10
  return Config { pool = p, vaultKey = vk }
  where connInfo cfg = 
          defaultConnectInfo  { connectDatabase = dbName cfg
                              , connectHost     = dbHost cfg
                              , connectUser     = dbUsername cfg
                              , connectPassword = dbPassword cfg
                              }

readRawConfig = do
  dbName  <- lookupEnv "LIVELOG_DB_NAME"
  dbUname <- lookupEnv "LIVELOG_DB_USERNAME"
  dbPassw <- lookupEnv "LIVELOG_DB_PASSWORD"
  dbHost  <- return . return $ "db"
  let cfg = RawConfig <$> dbHost
                      <*> dbName
                      <*> dbUname
                      <*> dbPassw
  case cfg of
    Nothing -> error "No config found"
    Just v  -> return v

clearDB :: Config -> IO ()
clearDB c = runSqlPool clearModels (pool c)