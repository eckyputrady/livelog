{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Tags (routes) where

import GHC.Generics
import qualified Data.Aeson as Aeson
import Web.Scotty.Trans
import qualified Database.Persist as DB
import Database.Persist.Sql (toSqlKey)
import Network.HTTP.Types (status201)
import Data.Time
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))

import Types
import Model
import Controllers.Common

data CParam = CParam { name :: String } deriving (Generic)
instance Aeson.ToJSON CParam
instance Aeson.FromJSON CParam

routes :: AppM ()
routes = do
  get     "/tags"     $ requireUser >>= _query
  post    "/tags"     $ requireUser >>= _save
  get     "/tags/:id" $ requireUser >>= _get
  put     "/tags/:id" $ requireUser >>= _update
  delete  "/tags/:id" $ requireUser >>= _delete
  where
    _query userId = do
      (items :: [DB.Entity Tag]) <- withDB $ getFor TagUserId userId
      json items

    _get userId = do
      i   <- param "id"
      item <- withDB $ getUserItem TagUserId userId TagId (toKey (i :: Int))
      maybe (raise NotFound) json item

    _save userId = do
      d <- jsonData
      b <- liftIO $ toModel userId d
      l <- withDB $ DB.insert (b :: Tag)
      status status201
      json l

    _update userId = do
      d <- jsonData
      b <- liftIO $ toModel userId d
      i <- param "id"
      item <- withDB $ getUserItem TagUserId userId TagId (toKey (i :: Int))
      case item of
        Nothing -> raise NotFound
        Just _  -> withDB $ DB.replace (toKey (i :: Int)) (b :: Tag)

    _delete userId = do
      i <- param "id"
      item <- withDB $ getUserItem TagUserId userId TagId (toKey (i :: Int))
      case item of
        Nothing -> raise NotFound
        Just _  -> withDB $ DB.delete $ (toKey (i :: Int) :: DB.Key Tag)

toModel :: UserId -> CParam -> IO Tag
toModel userId p = do
  return $ Tag userId (name p)