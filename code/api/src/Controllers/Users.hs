{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Users (routes) where

import GHC.Generics
import qualified Data.Aeson as Aeson
import Web.Scotty.Trans
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Network.HTTP.Types (status201)
import Data.Time
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))
import Data.Serialize (encode, decode)
import qualified Database.Persist as DB

import Types
import Model
import Controllers.Common

routes :: AppM ()
routes =
  post "/users" save

save :: ActM ()
save = do
  b <- jsonDataE
  l <- withDB (DB.insert (b :: User)) `rescue` (\(Unhandled msg) -> raise $ BadRequest msg)
  status status201
  json l
