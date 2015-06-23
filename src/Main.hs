{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, lift, runReaderT)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Text.Lazy (Text, pack)
import Data.ByteString (ByteString)
import Prelude
import Web.Scotty.Trans
import qualified Database.Persist as DB
import Database.Persist.MySQL (ConnectionPool, createMySQLPool, defaultConnectInfo, connectDatabase, connectPassword)
import Database.Persist.Sql (runSqlPool, fromSqlKey, toSqlKey)
import Network.HTTP.Types (status404, status201)
import Network.Wai (Middleware(..), vault)
import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified Data.Vault.Lazy as Vault
import Web.ClientSession (getDefaultKey)
import Network.Wai.Session (withSession, Session, SessionStore)
import Network.Wai.Session.ClientSession (clientsessionStore)
import Data.Serialize (encode, decode)

import Model
import Types
import Controllers.Common
import qualified Controllers.Logs as CLogs

application :: [Middleware] -> AppM ()
application mws = do
  -- middlewares
  mapM_ middleware mws

  -- routes
  sessionRoutes
  userRoutes
  CLogs.routes
  tagRoutes
  notFound notFoundA

main :: IO ()
main = do 
  c   <- getConfig
  mws <- sequence [sessionMW c]
  runSqlPool migrateModels (pool c)
  scottyOptsT def (runIO c) $ application mws
  where
    runIO :: Config -> ConfigM a -> IO a
    runIO c m = runReaderT (runConfigM m) c

    sessionMW :: Config -> IO Middleware
    sessionMW c = do
      sstore <- clientsessionStore <$> getDefaultKey
      return $ withSession sstore "session" def (vaultKey c)

getConfig = do
  p   <- runStdoutLoggingT $ createMySQLPool connInfo 10
  vk  <- Vault.newKey
  return $ Config { pool = p, vaultKey = vk }
  where connInfo = defaultConnectInfo { connectDatabase = "livelog", connectPassword = "" }

--

sessionRoutes :: AppM ()
sessionRoutes = do
  post    "/sessions"   _save
  delete  "/sessions"   _delete
  where
    _save = do
      d <- jsonData
      user <- withDB $ DB.getByValue (d :: User)
      case user of
        Nothing -> 
          notFoundA
        Just v -> do
          (_, sessionInsert) <- getSession
          liftIO $ sessionInsert "u" $ encode . fromSqlKey . DB.entityKey $ v 
          status status201

    _delete = do
      (_, sessionInsert) <- getSession
      liftIO $ sessionInsert "u" ""


userRoutes :: AppM ()
userRoutes = do
  post "/users"   _save
  where
    _save = do
      b <- jsonData
      l <- withDB $ DB.insert (b :: User)
      status status201
      json l

tagRoutes :: AppM ()
tagRoutes = do
  get   "/tags"     _query
  post  "/tags"     _save
  get   "/tags/:id" _get
  where
    _query = do
      (tags :: [DB.Entity Tag]) <- withDB $ DB.selectList [] []
      json tags

    _get = do
      i   <- param "id"
      log <- withDB . DB.get . toSqlKey . fromIntegral $ (i :: Int)
      json (log :: Maybe Log)

    _save = do
      b <- jsonData
      l <- withDB $ DB.insert (b :: Tag)
      status status201
      json l


