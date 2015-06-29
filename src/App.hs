{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where

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
import Database.Persist.Sql (ConnectionPool, runSqlPool, fromSqlKey, toSqlKey)
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
import qualified Controllers.Tags as CTags
import qualified Controllers.LogTag as CLogTag

application :: [Middleware] -> AppM ()
application mws = do
  -- middlewares
  mapM_ middleware mws

  -- routes
  sessionRoutes
  userRoutes
  CLogs.routes
  CTags.routes
  CLogTag.routes
  notFound notFoundA

-- runApp :: Config -> IO ()
runApp c runner = do
  runSqlPool migrateModels (pool c)
  mws <- sequence [sessionMW c]
  runner (def :: Options) (runIO c) $ application mws
  where
    runIO :: Config -> ConfigM a -> IO a
    runIO c m = runReaderT (runConfigM m) c

    sessionMW :: Config -> IO Middleware
    sessionMW c = do
      sstore <- clientsessionStore <$> getDefaultKey
      return $ withSession sstore "session" def (vaultKey c)

ioRunner a b c = scottyOptsT a b c
appRunner _ = scottyAppT 

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

