{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controllers.Logs (routes) where

import           Control.Applicative    ((<$>))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson             as Aeson
import           Data.Maybe             (mapMaybe)
import           Data.Text              (splitOn)
import           Data.Time
import qualified Database.Persist       as DB
import           Database.Persist.Sql   (toSqlKey)
import           GHC.Generics
import           Network.HTTP.Types     (status201)
import           Web.Scotty.Trans

import           Controllers.Common
import           Model
import           Types

data CParam = CParam { message :: String, time :: Maybe UTCTime } deriving (Generic)
instance Aeson.ToJSON CParam
instance Aeson.FromJSON CParam

routes :: AppM ()
routes = do
  get     "/logs"     query
  post    "/logs"     save
  get     "/logs/:id" getOne
  put     "/logs/:id" update
  delete  "/logs/:id" remove

query :: ActM ()
query = do
  userId <- getUserId
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

save :: ActM ()
save = do
  userId <- getUserId
  b <- parseModel userId
  l <- withDB $ DB.insert (b :: Log)
  status status201
  json l

getOne :: ActM ()
getOne = do
  userId <- getUserId
  (_, log) <- getLog userId
  maybe (raise NotFound) json log

update :: ActM ()
update = do
  userId <- getUserId
  b <- parseModel userId
  (key, log) <- getLog userId
  case log of
    Nothing -> raise NotFound
    Just _  -> withDB $ DB.replace key b

remove :: ActM ()
remove = do
  userId <- getUserId
  (key, log) <- getLog userId
  case log of
    Nothing -> raise NotFound
    Just _  -> withDB $ DB.delete key

parseModel userId = do
  userId <- getUserId
  d <- jsonData
  liftIO $ toModel userId d

getLog userId = do
  i <- param "id"
  let key = toKey (i :: Int)
  log <- withDB $ getUserItem LogUserId userId LogId key
  return (key, log)

toModel :: UserId -> CParam -> IO Log
toModel userId p = do
  t <- maybe getCurrentTime return (time p)
  return $ Log userId (message p) t
