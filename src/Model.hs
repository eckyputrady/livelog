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

import Data.Text (Text)
import Data.Maybe (listToMaybe)
import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Data.Time (UTCTime)
import Control.Monad.IO.Class (MonadIO(..))
import Database.Esqueleto
import Database.Persist.TH
import Database.Persist.MySQL (ConnectionPool, createMySQLPool, defaultConnectInfo, ConnectInfo(..))
import Database.Persist.Sql (runSqlPool)
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

-- running the migration
migrateModels :: (MonadIO m) => SqlPersistT m ()
migrateModels = runMigration migrateAll 

toKey x = toSqlKey . fromIntegral $ x
getById x = get . toKey $ x

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

logQuery :: (MonadIO m) => LogFilterParam -> LogSortParam -> SqlPersistT m [Entity Log]
logQuery filterParam sortParam = 
  select $ from $ \row -> do
  where_ (msgCondition row &&. sinceCondition row)
  limitCondition
  orderBy $ map (orderCondition row) sortParam
  return row
  where
    msgCondition row = case (lfpMessage filterParam) of
      Nothing -> val True
      Just msg -> ((row ^. LogMessage) `like` concat_ [(%), val msg, (%)])
    sinceCondition row = case (lfpSinceId filterParam) of
      Nothing -> val True
      Just sinceId -> (row ^. LogId >=. val sinceId)
    limitCondition = limit $ maybe 50 id (lfpLimit filterParam)
    orderCondition row (LogSortCreatedAt dir) = parseDir dir $ (row ^. LogCreatedAt)
    orderCondition row (LogSortMessage dir) = parseDir dir $ (row ^. LogMessage)
    parseDir Asc = asc
    parseDir Desc = desc

queryLogTags userId mLogId mTagId =
  select $ from $ \(log `InnerJoin` taglog `InnerJoin` tag) -> do
  on (tag ^. TagId ==. taglog ^. TagLogTagId)
  on (log ^. LogId ==. taglog ^. TagLogLogId)
  where_ (logIdCond taglog mLogId &&. tagIdCond taglog mTagId &&. userCond log tag)
  return (log, taglog, tag)
  where logIdCond _ Nothing       = val True
        logIdCond taglog (Just v) = (taglog ^. TagLogLogId ==. val v)
        tagIdCond _ Nothing       = val True
        tagIdCond taglog (Just v) = (taglog ^. TagLogTagId ==. val v)
        userCond log tag = log ^. LogUserId ==. val userId &&. tag ^. TagUserId ==. val userId

delLogTags logId tagId =
  delete $ from $ \taglog -> do
  where_ (taglog ^. TagLogLogId ==. val logId &&. taglog ^. TagLogTagId ==. val tagId)


-- LOGS

-- getLogsFor :: Entity User -> SqlPersistT m [Entity Log]
-- getLogsFor userId =
--   select $ from $ \log -> do
--   where_ (log ^. LogUserId ==. val userId)
--   return log

getFor getterF itemId =
  select $ from $ \item -> do
  where_ (item ^. getterF ==. val itemId)
  return item

getWithCond xs = 
  select $ from $ \item -> do
  where_ (foldr (&&.) (val True) $ map (parseCond item) xs)
  return item
  where 
    parseCond item (getterF, getterVal) =
      item ^. getterF ==. val getterVal

getUserItem userF userId itemF itemId =
  listToMaybe <$> results
  where results = select $ from $ \item -> do
                  where_ (item ^. itemF ==. val itemId &&. item ^. userF ==. val userId)
                  limit 1
                  return item

delWithCond xs = 
  delete $ from $ \item -> do
  where_ (foldr (&&.) (val True) $ map (parseCond item) xs)
  where 
    parseCond item (getterF, getterVal) =
      item ^. getterF ==. val getterVal

-- /// Users

-- createUser u = do
--   i <- insert (u :: User)
--   get i

-- getUser u = do
--   x <-  select $ from $ \user -> do
--         where_ (user ^. UserName ==. u ^. UserName)
--         limit 1
--         return log
--   return $ case x of 
--     []  -> Nothing
--     [v] -> Just v

-- -- createSession = 

-- -- /// LOGS

-- -- get last n logs
-- getLogs n = 
--   select $ from $ \log -> do
--   orderBy [desc (log ^. LogCreatedAt)]
--   limit n
--   return $ log

-- -- get log
-- getLog id = do
--   x <-  select $ from $ \log -> do
--         where_ (log ^. LogId ==. val (toSqlKey id))
--         limit 1
--         return log
--   return $ case x of 
--     []  -> Nothing
--     [v] -> Just v

-- -- createLog
-- createLog log = do
--   i <- insert log
--   get i

-- -- create log
-- -- createLogWithTag msg tagName time = do
-- --   maybeL <- createLog (Log msg time)
-- --   case maybeL of
-- --     Just l  -> tagLog (entityKey l) tagName
-- --     Nothing -> Nothing

-- -- edit log msg
-- editLog logId msg time =
--   update $ \log -> do
--   set log (msgUpdate msg <> timeUpdate time)
--   where_ (log ^. LogId ==. val (toSqlKey logId))
--   where
--     msgUpdate (Just msg) = [LogMessage =. val msg]
--     msgUpdate _ = []
--     timeUpdate (Just time) = [LogCreatedAt =. val time]
--     timeUpdate _ = []

-- -- //// TAGS

-- -- get tags
-- getTags n =
--   select $ from $ \tag -> do
--   orderBy [asc (tag ^. TagId)]
--   limit n
--   return $ tag

-- -- get tag
-- getTag key = do 
--   let k = toSqlKey key :: TagId
--   v <- get k
--   return $ Entity k <$> v
-- getTagByName tagName = getBy $ UniqueTagUserIdName tagName 

-- -- create tag
-- createTag tagName = do
--   maybeTag <- getTagByName tagName
--   case maybeTag of
--     Nothing -> insert $ Tag tagName
--     Just v  -> return $ entityKey v

-- -- edit tag name
-- editTag tagId tagName =
--   update $ \tag -> do
--   set tag [TagName =. val tagName]
--   where_ (tag ^. TagId ==. val tagId)

-- -- //// TagLog

-- -- add log tagging
-- tagLog logId tagName = do
--   tagId <- createTag tagName
--   insert $ TagLog tagId logId

-- -- rem log tagging
-- untagLog logId tagName = do
--   maybeTag <- getTagByName tagName
--   case maybeTag of
--     Nothing -> return ()
--     Just v  -> delete $ from $ \tagging -> do
--                 where_ (tagging ^. TagLogTagId ==. val (entityKey v) &&. tagging ^. TagLogLogId ==. val logId)