{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.LogTag (routes) where

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

data CParam = CParam { logId :: Int, tagId :: Int } deriving (Generic)
instance Aeson.ToJSON CParam
instance Aeson.FromJSON CParam

routes :: AppM ()
routes = do
  tagLogRoutes
  logRoutes
  tagRoutes

tagLogRoutes :: AppM ()
tagLogRoutes =
  post  "/taglog" $ requireUser >>= _save
  where
    _save userId = do
      d <- jsonData
      let _logId = toKey . logId $ d
      let _tagId = toKey . tagId $ d
      log <- withDB $ getUserItem LogUserId userId LogId _logId
      tag <- withDB $ getUserItem TagUserId userId TagId _tagId
      case (log, tag) of
        (Nothing, _) -> raise $ BadRequest "log not found"
        (_, Nothing) -> raise $ BadRequest "tag not found"
        (_, _) -> do  _save' (toModel userId d) _logId _tagId
                      status status201

    _save' model logId tagId = do 
      tagLog <- withDB $ getUserItem TagLogTagId tagId TagLogLogId logId
      case tagLog of
        Nothing -> do withDB $ DB.insert model
                      withDB $ getUserItem TagLogTagId tagId TagLogLogId logId
        Just _  -> return tagLog

logRoutes :: AppM ()
logRoutes = do
  get     "/logs/:logId/tags"         $ requireUser >>= _query
  delete  "/logs/:logId/tags/:tagId"  $ requireUser >>= delLogTag
  where
    _query userId = do
      i <- param "logId"
      results <- withDB $ queryLogTags userId (Just (toKey (i :: Int))) Nothing
      json $ map (\(_, _, tag) -> tag) results
      

tagRoutes :: AppM ()
tagRoutes = do
  get     "/tags/:tagId/logs"         $ requireUser >>= _query
  delete  "/tags/:tagId/logs/:logId"  $ requireUser >>= delLogTag
  where
    _query userId = do
      i <- param "tagId"
      results <- withDB $ queryLogTags userId Nothing (Just (toKey (i :: Int)))
      json $ map (\(log, _, _) -> log) results

delLogTag userId = do
  logId <- param "logId"
  tagId <- param "tagId"
  let logKey = toKey (logId :: Int)
  let tagKey = toKey (tagId :: Int)
  log <- withDB $ getUserItem LogUserId userId LogId logKey
  tag <- withDB $ getUserItem TagUserId userId TagId tagKey
  case (log, tag) of
    (Just _, Just _) -> withDB $ delLogTags logKey tagKey
    _ -> raise NotFound

toModel :: UserId -> CParam -> TagLog
toModel userId p = TagLog (toKey . tagId $ p) (toKey . logId $ p)