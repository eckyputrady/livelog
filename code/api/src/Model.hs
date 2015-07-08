{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}

module Model where

import Data.Maybe (listToMaybe, fromMaybe)
import Data.Text (Text)
import Control.Applicative ((<$>))
import Data.Time (UTCTime)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Database.Esqueleto
import Database.Persist.TH
import GHC.Int (Int64)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  name String
  pass String
  UniqueUnamePass name pass
  UniqueUname name
  deriving Show

Tag json
  userId  UserId
  name    String
  UniqueTagUserIdName name
  deriving Show

Log json
  userId UserId
  message String
  createdAt UTCTime
  deriving Show

TagLog json
  tagId TagId
  logId LogId
  UniqueTagLog tagId logId
  deriving Show
|]

-- Migration replated

migrateModels :: (MonadIO m, MonadBaseControl IO m) => SqlPersistT m [Text]
migrateModels = runMigrationSilent migrateAll

clearModels :: (MonadIO m) => SqlPersistT m ()
clearModels = rawExecute "DROP TABLE IF EXISTS tag_log, tag, log, user" []

-- Common func

toKey x = toSqlKey . fromIntegral $ x
getById x = get . toKey $ x

getFor getterF itemId =
  select $ from $ \item -> do
  where_ (item ^. getterF ==. val itemId)
  return item

getUserItem userF userId itemF itemId =
  listToMaybe <$> results
  where results = select $ from $ \item -> do
                  where_ (item ^. itemF ==. val itemId &&. item ^. userF ==. val userId)
                  limit 1
                  return item

delWithCond xs = 
  delete $ from $ \item ->
  where_ $ foldr ((&&.) . parseCond item) (val True) xs
  where 
    parseCond item (getterF, getterVal) =
      item ^. getterF ==. val getterVal

-- LOGS

data LogFilterParam = LogFilterParam
  { lfpMessage :: Maybe String
  , lfpLimit :: Maybe Int64
  , lfpSinceId :: Maybe (Key Log)
  }
type LogSortParam = [LogSortFieldParam]
data LogSortFieldParam  = LogSortCreatedAt SortDirection
                        | LogSortMessage SortDirection
data LogSaveParam = LogSaveParam
  { lspMessage :: String
  }
data SortDirection = Asc | Desc

logQuery :: (MonadIO m) => Key User -> LogFilterParam -> LogSortParam -> SqlPersistT m [Entity Log]
logQuery userId filterParam sortParam = 
  select $ from $ \row -> do
  where_ (userCondition row &&. msgCondition row &&. sinceCondition row)
  limitCondition
  orderBy $ map (orderCondition row) sortParam
  return row
  where
    userCondition row = row ^. LogUserId ==. val userId
    msgCondition row = case lfpMessage filterParam of
      Nothing -> val True
      Just msg -> (row ^. LogMessage) `like` concat_ [(%), val msg, (%)]
    sinceCondition row = case lfpSinceId filterParam of
      Nothing -> val True
      Just sinceId -> row ^. LogId >=. val sinceId
    limitCondition = limit $ fromMaybe 50 (lfpLimit filterParam)
    orderCondition row (LogSortCreatedAt dir) = parseDir dir $ row ^. LogCreatedAt
    orderCondition row (LogSortMessage dir) = parseDir dir $ row ^. LogMessage
    parseDir Asc = asc
    parseDir Desc = desc

-- LOG TAGS

queryLogTags userId mLogId mTagId =
  select $ from $ \(log `InnerJoin` taglog `InnerJoin` tag) -> do
  on (tag ^. TagId ==. taglog ^. TagLogTagId)
  on (log ^. LogId ==. taglog ^. TagLogLogId)
  where_ (logIdCond taglog mLogId &&. tagIdCond taglog mTagId &&. userCond log tag)
  return (log, taglog, tag)
  where logIdCond _ Nothing       = val True
        logIdCond taglog (Just v) = taglog ^. TagLogLogId ==. val v
        tagIdCond _ Nothing       = val True
        tagIdCond taglog (Just v) = taglog ^. TagLogTagId ==. val v
        userCond log tag = log ^. LogUserId ==. val userId &&. tag ^. TagUserId ==. val userId

delLogTags logId tagId =
  delete $ from $ \taglog ->
  where_ (taglog ^. TagLogLogId ==. val logId &&. taglog ^. TagLogTagId ==. val tagId)