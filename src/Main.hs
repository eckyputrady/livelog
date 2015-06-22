{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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

data Config = Config
  { pool :: ConnectionPool
  , vaultKey :: Vault.Key (Session IO ByteString ByteString)
  }

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

type AppM = ScottyT Text ConfigM
type ActM = ActionT Text ConfigM

application :: [Middleware] -> AppM ()
application mws = do
  -- middlewares
  mapM_ middleware mws

  -- routes
  sessionRoutes
  userRoutes
  logRoutes
  tagRoutes
  notFound notFoundA

notFoundA = do
  status status404

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

withDB q = do
  p <- lift $ asks pool
  liftIO $ runSqlPool q p

---

getSession = do
  vk <- lift $ asks vaultKey
  req <- request
  return $ Vault.lookup vk (vault req)

requireUser :: ActM (Maybe UserId)
requireUser = do
  Just (sessionLookup, _) <- getSession
  maybeUId <- liftIO $ sessionLookup "u"
  case maybeUId of 
    Nothing -> raise "Unknown"
    Just v -> case decode v of
      Left _ -> raise "Format error"
      Right decoded -> do
        let k = toSqlKey decoded
        mUser <- withDB $ DB.get (k :: UserId)
        liftIO $ putStrLn $ "you are " ++ (show mUser)
        case mUser of
          Nothing -> raise "Unknown user"
          Just user  -> do
            return $ Just k

sessionRoutes :: AppM ()
sessionRoutes = do
  post    "/sessions"   _save
  delete  "/sessions"   _delete
  where
    _save = do
      d <- jsonData
      vk <- lift $ asks vaultKey
      req <- request
      let Just (_, sessionInsert) = Vault.lookup vk (vault req)
      user <- withDB $ DB.getByValue (d :: User)
      case user of
        Nothing -> 
          notFoundA
        Just v -> do
          liftIO $ sessionInsert "u" $ encode . fromSqlKey . DB.entityKey $ v 
          status status201

    _delete = do
      Just (_, sessionInsert) <- getSession
      liftIO $ sessionInsert "u" ""


userRoutes :: AppM ()
userRoutes = do
  post "/users"   _save
  where
    _save = do
      b <- jsonData
      l <- withDB $ createUser (b :: User)
      status status201
      json l

logRoutes :: AppM ()
logRoutes = do
  get "/logs"     $ requireUser >>= _query
  post "/logs"    _save
  get "/logs/:id" _get
  where
    _query user = do
      logs <- withDB (getLogs 10)
      json logs

    _get = do
      i   <- param "id"
      log <- withDB $ getLog . fromIntegral $ (i :: Int)
      case log of
        Nothing -> notFoundA
        Just v  -> json v

    _save = do
      b <- jsonData
      l <- withDB $ createLog (b :: Log)
      status status201
      json l

tagRoutes :: AppM ()
tagRoutes = do
  get   "/tags"     _query
  post  "/tags"     _save
  get   "/tags/:id" _get
  where
    _query = do
      tags <- withDB (getTags 10)
      json tags

    _get = do
      i   <- param "id"
      log <- withDB . DB.get . toSqlKey . fromIntegral $ (i :: Int)
      json (log :: Maybe Log)

    _save = do
      b <- jsonData
      l <- withDB $ createLog (b :: Tag)
      status status201
      json l


