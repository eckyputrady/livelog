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
  post    "/tags/:id" $ requireUser >>= _update
  delete  "/tags/:id" $ requireUser >>= _delete
  where
    _query userId = do
      (items :: [DB.Entity Tag]) <- withDB $ getFor TagUserId userId
      json items

    _get _ = do
      i   <- param "id"
      (log :: Maybe Tag) <- withDB $ getById (i :: Int)
      json log

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
      l <- withDB $ DB.replace (toKey (i :: Int)) (b :: Tag)
      json l

    _delete userId = do
      i <- param "id"
      withDB $ DB.delete $ (toKey (i :: Int) :: DB.Key Tag)

toModel :: UserId -> CParam -> IO Tag
toModel userId p = do
  return $ Tag userId (name p)