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
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT(..), asks)
import Data.Text (Text)
import Database.Esqueleto
import Database.Persist.TH
import Database.Persist.MySQL (ConnectionPool, createMySQLPool, defaultConnectInfo, ConnectInfo(..))
import Database.Persist.Sql (runSqlPool)

type AppM = ReaderT Config (LoggingT IO)

data Config = Config 
  { getPool :: ConnectionPool }

defaultConfig :: IO Config
defaultConfig = do
  pool <- createPool
  return $ Config { getPool = pool
                  }
createPool :: IO ConnectionPool
createPool = runStdoutLoggingT $
  flip createMySQLPool 10 $ defaultConnectInfo  { connectUser = "root"
                                                , connectPassword = "root"
                                                , connectDatabase = "livelog"
                                                } 

run :: AppM a -> Config -> IO a
run app cfg = 
  runStdoutLoggingT $ runReaderT app cfg

main :: IO ()
main = do
  cfg <- defaultConfig
  run app cfg

app :: AppM a
app = undefined

-- runDB :: AppM a
runDB query = do
  pool <- asks getPool
  runSqlPool query pool


-- Model

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Tag
  name  String
  UniqueTagName name
  deriving Show

Log
  message String
  createdAt UTCTime
  deriving Show

TagLog
  tagId TagId
  logId LogId
  UniqueTagLog tagId logId
  deriving Show
|]