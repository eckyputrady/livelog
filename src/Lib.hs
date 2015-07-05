{-# LANGUAGE OverloadedStrings #-}

module Lib (main, getConfig, runAppIO, runApp, clearDB) where

import qualified Data.ByteString.Lazy as B
import Control.Monad.Logger (runStdoutLoggingT, runNoLoggingT)
import Database.Persist.MySQL (createMySQLPool, runSqlPool, defaultConnectInfo, connectDatabase, connectPassword)
import qualified Data.Vault.Lazy as Vault
import App
import Types
import Model
import qualified Data.Aeson as Aeson

main :: IO ()
main = do 
  c <- getConfig
  runAppIO c

getConfig = do
  rawCfg <- readRawConfig
  p <- runNoLoggingT $ createMySQLPool (connInfo rawCfg) 10
  vk <- Vault.newKey
  return $ Config { pool = p, vaultKey = vk }
  where connInfo cfg = 
          defaultConnectInfo  { connectDatabase = (db_name cfg)
                              , connectPassword = (db_password cfg) }

readRawConfig = do
  content <- B.readFile "config.json"
  case Aeson.decode content of
    Nothing -> error "Config failed to be parsed"
    Just rawCfg -> return rawCfg

clearDB :: Config -> IO ()
clearDB c = runSqlPool clearModels (pool c)