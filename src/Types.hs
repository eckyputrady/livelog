{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Applicative
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.ByteString (ByteString)
import Data.Text.Lazy (Text)
import Database.Persist.MySQL (ConnectionPool)
import qualified Data.Vault.Lazy as Vault
import Network.Wai.Session (Session)
import Web.Scotty.Trans (ScottyT, ActionT)

data Config = Config
  { pool :: ConnectionPool
  , vaultKey :: Vault.Key (Session IO ByteString ByteString)
  }

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

type AppM = ScottyT Text ConfigM
type ActM = ActionT Text ConfigM