{-# LANGUAGE OverloadedStrings #-}

module App (runApp, runAppIO) where

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
import Network.HTTP.Types (status404, status201, status400)
import Network.Wai (Middleware(..), vault, Response(..))
import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified Data.Vault.Lazy as Vault
import Web.ClientSession (getDefaultKey)
import Network.Wai.Session (withSession, Session, SessionStore)
import Network.Wai.Session.ClientSession (clientsessionStore)
import Data.Serialize (encode, decode)

import Model
import Types
import Controllers.Common
import qualified Controllers.Sessions as CSessions
import qualified Controllers.Users as CUsers
import qualified Controllers.Logs as CLogs
import qualified Controllers.Tags as CTags
import qualified Controllers.LogTag as CLogTag
import qualified Controllers.Error as CError

application :: [Middleware] -> AppM ()
application mws = do
  -- middlewares
  mapM_ middleware mws

  -- error handling
  defaultHandler CError.handler

  -- routes
  CSessions.routes
  CUsers.routes
  CLogs.routes
  CTags.routes
  CLogTag.routes
  notFound $ raise NotFound

runAppIO = run (scottyOptsT (def :: Options))

runApp = run scottyAppT

run runner c = do
  runSqlPool migrateModels (pool c)
  mws <- sequence [sessionMW c]
  runner runIO $ application mws
  where
    runIO :: ConfigM a -> IO a
    runIO m = runReaderT (runConfigM m) c

    sessionMW :: Config -> IO Middleware
    sessionMW c = do
      sstore <- clientsessionStore <$> getDefaultKey
      return $ withSession sstore "session" def (vaultKey c)

