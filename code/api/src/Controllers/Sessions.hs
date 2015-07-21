{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controllers.Sessions (routes) where

import           Control.Applicative    ((<$>))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson             as Aeson
import           Data.Serialize         (decode, encode)
import           Data.Time
import qualified Database.Persist       as DB
import           Database.Persist.Sql   (fromSqlKey, toSqlKey)
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
  get     "/sessions"   getOne
  post    "/sessions"   save
  delete  "/sessions"   remove

getOne :: ActM ()
getOne = do
  userId <- getUserId
  user <- withDB $ DB.get userId
  case user of
    Nothing -> raise NotFound
    Just v -> json $ CParam (userName v)

save :: ActM ()
save = do
  (d :: User) <- jsonDataE
  user <- withDB $ getUserItem UserName (userName d) UserPass (userPass d)
  case user of
    Nothing ->
      raise $ BadRequest "user not found"
    Just v -> do
      (_, sessionInsert) <- getSession
      liftIO . sessionInsert "u" . encode . fromSqlKey . DB.entityKey $ v
      status status201

remove :: ActM ()
remove = do
  (_, sessionInsert) <- getSession
  liftIO $ sessionInsert "u" ""
