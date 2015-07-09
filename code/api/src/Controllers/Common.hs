{-# LANGUAGE OverloadedStrings #-}

module Controllers.Common where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (asks)
import           Control.Monad.Trans    (lift)
import           Data.Aeson             (FromJSON)
import           Data.Serialize         (decode)
import           Data.Text.Lazy         (Text)
import qualified Data.Vault.Lazy        as Vault
import qualified Database.Persist       as DB
import           Database.Persist.Sql   (runSqlPool, toSqlKey)
import           Network.HTTP.Types     (status404)
import           Network.Wai            (vault)
import           Web.Scotty.Trans

import           Model
import           Types

withDB q = do
  p <- lift $ asks pool
  liftIO $ runSqlPool q p

maybeParam :: (Parsable a) => Text -> [Param] -> Maybe a
maybeParam id params = do
  p <- lookup id params
  case parseParam p of
    Left _ -> Nothing
    Right v -> Just v

jsonDataE :: (FromJSON a) => ActM a
jsonDataE = jsonData `rescue` (\(Unhandled m) -> raise $ BadRequest m)

getSession = do
  vk <- lift $ asks vaultKey
  req <- request
  case Vault.lookup vk (vault req) of
    Nothing -> raise $ Unauthorized "Session unknown"
    Just x  -> return x

requireUser :: ActM UserId
requireUser = do
  (sessionLookup, _) <- getSession
  maybeUId <- liftIO $ sessionLookup "u"
  case maybeUId of
    Nothing -> raise $ Unauthorized "maybeUId is nothing"
    Just v -> case decode v of
      Left _ -> raise $ Unauthorized "Format error"
      Right decoded -> do
        let k = toSqlKey decoded
        mUser <- withDB $ DB.get (k :: UserId)
        case mUser of
          Nothing -> raise $ Unauthorized "Unknown user"
          Just user  -> return k

getUserId :: ActM UserId
getUserId = requireUser
