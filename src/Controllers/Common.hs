{-# LANGUAGE OverloadedStrings #-}

module Controllers.Common where

import Control.Monad.Trans (lift)
import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql (runSqlPool, toSqlKey)
import qualified Database.Persist as DB
import Web.Scotty.Trans
import qualified Data.Vault.Lazy as Vault
import Network.Wai (vault)
import Data.Serialize (decode)
import Network.HTTP.Types (status404)
import Data.Text.Lazy (Text)

import Model
import Types

withDB q = do
  p <- lift $ asks pool
  liftIO $ runSqlPool q p

maybeParam :: (Parsable a) => Text -> [Param] -> Maybe a
maybeParam id params = do
  p <- lookup id params
  case parseParam p of
    Left _ -> Nothing
    Right v -> Just v

notFoundA :: ActM ()
notFoundA = do
  status status404

getSession = do
  vk <- lift $ asks vaultKey
  req <- request
  case Vault.lookup vk (vault req) of
    Nothing -> raise "Session unknown"
    Just x  -> return x

requireUser :: ActM UserId
requireUser = do
  (sessionLookup, _) <- getSession
  maybeUId <- liftIO $ sessionLookup "u"
  case maybeUId of 
    Nothing -> raise "maybeUId is nothing"
    Just v -> case decode v of
      Left _ -> raise "Format error"
      Right decoded -> do
        let k = toSqlKey decoded
        mUser <- withDB $ DB.get (k :: UserId)
        liftIO $ putStrLn $ "you are " ++ (show mUser)
        case mUser of
          Nothing -> raise "Unknown user"
          Just user  -> return k