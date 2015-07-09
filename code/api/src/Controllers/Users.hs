{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controllers.Users (routes) where

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

routes :: AppM ()
routes =
  post "/users" save

save :: ActM ()
save = do
  b <- jsonDataE
  l <- withDB (DB.insert (b :: User)) `rescue` (\(Unhandled msg) -> raise $ BadRequest msg)
  status status201
  json l
