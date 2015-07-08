{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Logs (routes) where

import GHC.Generics
import qualified Data.Aeson as Aeson
import Web.Scotty.Trans
import qualified Database.Persist as DB
import Database.Persist.Sql (toSqlKey)
import Network.HTTP.Types (status201)
import Data.Time
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))
import Data.Text (splitOn)
import Data.Maybe (mapMaybe)

import Types
import Model
import Controllers.Common

data CParam = CParam { message :: String, time :: Maybe UTCTime } deriving (Generic)
instance Aeson.ToJSON CParam
instance Aeson.FromJSON CParam

routes :: AppM ()
routes = do
  get     "/logs"     $ requireUser >>= _query
  post    "/logs"     $ requireUser >>= _save
  get     "/logs/:id" $ requireUser >>= _get
  put     "/logs/:id" $ requireUser >>= _update
  delete  "/logs/:id" $ requireUser >>= _delete
  where
    _query userId = do
      ps <- params
      logs <- withDB $ logQuery userId (filterParam ps) (sortParam ps)
      json logs
      where filterParam ps = LogFilterParam { lfpMessage  = maybeParam "message" ps
                                            , lfpLimit    = do  i <- maybeParam "limit" ps
                                                                return $ fromIntegral (i :: Int)
                                            , lfpSinceId  = do  i <- maybeParam "sinceId" ps
                                                                return $ toKey (i :: Int)
                                            }
            sortParam ps = case maybeParam "sort" ps of
                              Nothing -> []
                              Just p  -> mapMaybe parseSort $ splitOn "," p
            parseSort "createdAt"   = Just $ LogSortCreatedAt Asc 
            parseSort "-createdAt"  = Just $ LogSortCreatedAt Desc
            parseSort "message"     = Just $ LogSortMessage   Asc 
            parseSort "-message"    = Just $ LogSortMessage   Desc
            parseSort _             = Nothing

    _get userId = do
      i   <- param "id"
      log <- withDB $ getUserItem LogUserId userId LogId (toKey (i :: Int))
      maybe (raise NotFound) json log

    _save userId = do
      d <- jsonData
      b <- liftIO $ toModel userId d
      l <- withDB $ DB.insert (b :: Log)
      status status201
      json l

    _update userId = do
      d <- jsonData
      b <- liftIO $ toModel userId d
      i <- param "id"
      log <- withDB $ getUserItem LogUserId userId LogId (toKey (i :: Int))
      case log of
        Nothing -> raise NotFound
        Just _  -> withDB $ DB.replace (toKey (i :: Int)) (b :: Log)

    _delete userId = do
      i <- param "id"
      log <- withDB $ getUserItem LogUserId userId LogId (toKey (i :: Int))
      case log of
        Nothing -> raise NotFound
        Just _  -> withDB $ DB.delete (toKey (i :: Int) :: DB.Key Log)

toModel :: UserId -> CParam -> IO Log
toModel userId p = do
  t <- maybe getCurrentTime return (time p)
  return $ Log userId (message p) t