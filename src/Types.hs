{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Applicative
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.ByteString (ByteString)
import Data.Text.Lazy (Text, pack)
import Database.Persist.Sql (ConnectionPool)
import qualified Data.Vault.Lazy as Vault
import Network.Wai.Session (Session)
import Web.Scotty.Trans (ScottyT, ActionT, ScottyError(..))

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