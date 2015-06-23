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

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Data.Time (UTCTime)
import Control.Monad.IO.Class (MonadIO(..))
import Database.Esqueleto
import Database.Persist.TH
import Database.Persist.MySQL (ConnectionPool, createMySQLPool, defaultConnectInfo, ConnectInfo(..))
import Database.Persist.Sql (runSqlPool)

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
  { lfpMessage :: Maybe Text
  , lfpLimit :: Maybe Integer
  , lfpSinceId :: Maybe Integer
  }
data LogSortParam = LogSortParam
  { lspCreatedAt :: Maybe SortDirection
  , lspMessage :: Maybe SortDirection
  }
data LogSaveParam :: LogSaveParam
  { lspMessage :: Text
  }
data SortDirection = Asc | Desc

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