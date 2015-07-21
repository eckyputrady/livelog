{-# LANGUAGE OverloadedStrings #-}

module App (runApp, runAppIO) where

import           Control.Applicative
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Logger              (runStdoutLoggingT)
import           Control.Monad.Reader              (MonadIO, MonadReader, ReaderT, asks, lift, runReaderT)
import           Control.Monad.Trans               (MonadTrans)
import           Data.ByteString                   (ByteString)
import           Data.Default.Class                (def)
import           Data.Text.Lazy                    (Text, pack)
import           Data.Text.Lazy.Encoding           (decodeUtf8)
import qualified Database.Persist                  as DB
import           Database.Persist.Sql              (ConnectionPool, fromSqlKey, runSqlPool, toSqlKey)
import           Network.HTTP.Types                (status201, status400, status404)
import           Network.Wai                       (Middleware(..), Response(..), vault)
import           Web.Scotty.Trans

import           Data.Serialize                    (decode, encode)
import qualified Data.Vault.Lazy                   as Vault
import           Network.Wai.Session               (Session, SessionStore, withSession)
import           Network.Wai.Session.ClientSession (clientsessionStore)
import           Web.ClientSession                 (getDefaultKey)
import           Network.Wai.Middleware.Static     (initCaching, CachingStrategy(..), staticPolicy', noDots, (>->), addBase)

import           Controllers.Common
import qualified Controllers.Error                 as CError
import qualified Controllers.LogTag                as CLogTag
import qualified Controllers.Logs                  as CLogs
import qualified Controllers.Sessions              as CSessions
import qualified Controllers.Tags                  as CTags
import qualified Controllers.Users                 as CUsers
import           Model
import           Types

application :: [Middleware] -> AppM ()
application mws = do
  -- middlewares
  mapM_ middleware mws

  -- error handling
  defaultHandler CError.handler

  -- routes
  get "/" $ serveFile "/index.html"
  CSessions.routes
  CUsers.routes
  CLogs.routes
  CTags.routes
  CLogTag.routes
  notFound $ raise NotFound

serveFile :: String -> ActM ()
serveFile pathStr = do
  path <- lift $ asks staticPath
  -- content <- liftIO . readFile $ path ++ pathStr
  -- html . pack $ content
  file $ path ++ pathStr

runAppIO = run (scottyOptsT (def :: Options))

runApp = run scottyAppT

run runner c = do
  runSqlPool migrateModels (pool c)
  mws <- sequence [sessionMW c, staticMW c]
  runner runIO $ application mws
  where
    runIO :: ConfigM a -> IO a
    runIO m = runReaderT (runConfigM m) c

    sessionMW :: Config -> IO Middleware
    sessionMW c = do
      sstore <- clientsessionStore <$> getDefaultKey
      return $ withSession sstore "session" def (vaultKey c)

    staticMW :: Config -> IO Middleware
    staticMW c = do
      caching <- initCaching PublicStaticCaching
      return $ staticPolicy' caching (noDots >-> addBase (staticPath c)) 

