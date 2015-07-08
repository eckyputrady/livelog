{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import qualified Data.Aeson as Aeson
import Control.Applicative
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.ByteString (ByteString)
import Data.Text.Lazy (Text, pack)
import Database.Persist.Sql (ConnectionPool)
import qualified Data.Vault.Lazy as Vault
import Network.Wai.Session (Session)
import Web.Scotty.Trans (ScottyT, ActionT, ScottyError(..))

data RawConfig = RawConfig
  { db_host :: String
  , db_name :: String
  , db_username :: String
  , db_password :: String
  } deriving (Generic)
instance Aeson.ToJSON RawConfig
instance Aeson.FromJSON RawConfig

data Config = Config
  { pool :: ConnectionPool
  , vaultKey :: Vault.Key (Session IO ByteString ByteString)
  }

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

data AppError = BadRequest String
              | Unauthorized String
              | NotFound
              | Unhandled String
              deriving (Show)

instance ScottyError AppError where
  stringError = Unhandled
  showError = pack . show

type AppM = ScottyT AppError ConfigM
type ActM = ActionT AppError ConfigM