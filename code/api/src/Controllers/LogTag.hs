{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controllers.LogTag (routes) where

import           Control.Applicative    ((<$>))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson             as Aeson
import           Data.Time
import qualified Database.Persist       as DB
import           Database.Persist.Sql   (toSqlKey)
import           GHC.Generics
import           Network.HTTP.Types     (status201)
import           Web.Scotty.Trans

import           Controllers.Common
import           Model
import           Types

data CParam = CParam { logId :: Int, tagId :: Int } deriving (Generic)
instance Aeson.ToJSON CParam
instance Aeson.FromJSON CParam

routes :: AppM ()
routes = do
  get     "/logs/:logId/tags"         queryTags
  post    "/logs/:logId/tags/:tagId"  save
  delete  "/logs/:logId/tags/:tagId"  remove

  get     "/tags/:tagId/logs"         queryLogs
  post    "/tags/:tagId/logs/:logId"  save
  delete  "/tags/:tagId/logs/:logId"  remove

queryLogs = do
  userId <- getUserId
  i <- param "tagId"
  results <- withDB $ queryLogTags userId Nothing (Just (toKey (i :: Int)))
  json $ map (\(log, _, _) -> log) results

queryTags = do
  userId <- getUserId
  i <- param "logId"
  results <- withDB $ queryLogTags userId (Just (toKey (i :: Int))) Nothing
  json $ map (\(_, _, tag) -> tag) results

remove = do
  (logKey, log, tagKey, tag) <- getLogTag
  case (log, tag) of
    (Just _, Just _) -> withDB $ delLogTags logKey tagKey
    _ -> raise NotFound

save = do
  (logKey, log, tagKey, tag) <- getLogTag
  case (log, tag) of
    (Nothing, _)  -> raise $ BadRequest "log not found"
    (_, Nothing)  -> raise $ BadRequest "tag not found"
    (_, _)        -> do _save' logKey tagKey
                        status status201
  where
    _save' logId tagId = do
      tagLog <- withDB $ getUserItem TagLogTagId tagId TagLogLogId logId
      case tagLog of
        Nothing -> do withDB $ DB.insert $ TagLog tagId logId
                      withDB $ getUserItem TagLogTagId tagId TagLogLogId logId
        Just _  -> return tagLog

getLogTag = do
  userId <- getUserId
  logId <- param "logId"
  tagId <- param "tagId"
  let logKey = toKey (logId :: Int) :: DB.Key Log
  let tagKey = toKey (tagId :: Int) :: DB.Key Tag
  log <- withDB $ getUserItem LogUserId userId LogId logKey
  tag <- withDB $ getUserItem TagUserId userId TagId tagKey
  return (logKey, log, tagKey, tag)

toModel :: UserId -> CParam -> TagLog
toModel userId p = TagLog (toKey . tagId $ p) (toKey . logId $ p)
