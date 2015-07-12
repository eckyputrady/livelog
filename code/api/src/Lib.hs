{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (main, getConfig, runAppIO, runApp, clearDB) where

import           Control.Applicative
import           Control.Concurrent     (threadDelay)
import           Control.Exception      (SomeException(..), catch, try)
import           Control.Monad.Logger   (runNoLoggingT, runStdoutLoggingT)
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as B
import           Data.Either
import qualified Data.Vault.Lazy        as Vault
import           Database.Persist.MySQL 
import           System.Environment
import           System.IO.Error

import           Model
import           App
import           Types

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
  return Config { pool = p, vaultKey = vk, staticPath = (staticFilesPath rawCfg) }
  where connInfo cfg =
          defaultConnectInfo  { connectDatabase = dbName cfg
                              , connectHost     = dbHost cfg
                              , connectUser     = dbUsername cfg
                              , connectPassword = dbPassword cfg
                              }

readRawConfig = do
  dbName          <- lookupEnv "LIVELOG_DB_NAME"
  dbUname         <- lookupEnv "LIVELOG_DB_USERNAME"
  dbPassw         <- lookupEnv "LIVELOG_DB_PASSWORD"
  dbHost          <- return . return $ "db"
  staticFilesPath <- lookupEnv "LIVELOG_STATIC_PATH"
  let cfg = RawConfig <$> dbHost
                      <*> dbName
                      <*> dbUname
                      <*> dbPassw
                      <*> staticFilesPath
  case cfg of
    Nothing -> error "No config found"
    Just v  -> return v

clearDB :: Config -> IO ()
clearDB c = runSqlPool clearModels (pool c)
