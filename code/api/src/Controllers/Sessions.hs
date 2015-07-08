{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Sessions (routes) where

import GHC.Generics
import qualified Data.Aeson as Aeson
import Web.Scotty.Trans
import qualified Database.Persist as DB
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Network.HTTP.Types (status201)
import Data.Time
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))
import Data.Serialize (encode, decode)
import qualified Database.Persist as DB

import Types
import Model
import Controllers.Common

data CParam = CParam { name :: String } deriving (Generic)
instance Aeson.ToJSON CParam
instance Aeson.FromJSON CParam

routes :: AppM ()
routes = do
  get     "/sessions"   $ requireUser >>= _get
  post    "/sessions"   $ _save
  delete  "/sessions"   $ _delete
  where
    _get userId = do
      user <- withDB $ DB.get userId
      case user of
        Nothing -> raise NotFound
        Just v -> json $ CParam (userName v)

    _save = do
      d <- jsonDataE
      user <- withDB $ DB.getByValue (d :: User)
      case user of
        Nothing -> 
          raise $ BadRequest "user not found"
        Just v -> do
          (_, sessionInsert) <- getSession
          liftIO $ sessionInsert "u" $ encode . fromSqlKey . DB.entityKey $ v 
          status status201

    _delete = do
      (_, sessionInsert) <- getSession
      liftIO $ sessionInsert "u" ""

toModel :: UserId -> CParam -> IO Tag
toModel userId p = do
  return $ Tag userId (name p)