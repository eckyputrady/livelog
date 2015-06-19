{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import Control.Monad.Logger (MonadLogger, LoggingT(..), runStdoutLoggingT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Text (Text)
import Database.Persist.MySQL (ConnectionPool, createMySQLPool, defaultConnectInfo, ConnectInfo(..))

type AppM = ReaderT Config (LoggingT IO)

data Config = Config 
  { getPool :: ConnectionPool }

-- defaultConfig :: (MonadIO m, MonadBaseControl a m, MonadLogger m) => m Config
defaultConfig = do
  pool <- createPool
  return $ Config { getPool = pool
                  }

-- createPool :: (MonadIO m, MonadBaseControl a m, MonadLogger m) => m ConnectionPool
createPool = 
  flip createMySQLPool 10 $ defaultConnectInfo  { connectUser = "root"
                                                , connectPassword = "root"
                                                , connectDatabase = "livelog"
                                                } 

run :: AppM a -> m Config -> IO a
run app cfgM = runStdoutLoggingT $ do
  cfg <- cfgM
  return (runReaderT app cfg)

main :: IO ()
main = 
  run app defaultConfig

app :: AppM ()
app = undefined