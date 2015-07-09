{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controllers.Tags (routes) where

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

data CParam = CParam { name :: String } deriving (Generic)
instance Aeson.ToJSON CParam
instance Aeson.FromJSON CParam

routes :: AppM ()
routes = do
  get     "/tags"     query
  post    "/tags"     save
  get     "/tags/:id" getOne
  put     "/tags/:id" update
  delete  "/tags/:id" remove

query :: ActM ()
query = do
  userId <- getUserId
  (items :: [DB.Entity Tag]) <- withDB $ getFor TagUserId userId
  json items

getOne :: ActM ()
getOne = do
  userId  <- getUserId
  (_, item) <- getTag userId
  maybe (raise NotFound) json item

save :: ActM ()
save = do
  userId <- getUserId
  b <- parseModel userId
  l <- withDB $ DB.insert b
  status status201
  json l

update :: ActM ()
update = do
  userId <- getUserId
  b <- parseModel userId
  (key, item) <- getTag userId
  case item of
    Nothing -> raise NotFound
    Just _  -> withDB $ DB.replace key b

remove :: ActM ()
remove = do
  userId <- getUserId
  (key, item) <- getTag userId
  case item of
    Nothing -> raise NotFound
    Just _  -> withDB $ DB.delete key

getTag userId = do
  i <- param "id"
  let key = toKey (i :: Int)
  item <- withDB $ getUserItem TagUserId userId TagId key
  return (key, item)

parseModel userId = do
  d <- jsonData
  liftIO $ toModel userId d

toModel :: UserId -> CParam -> IO Tag
toModel userId p = return $ Tag userId (name p)
