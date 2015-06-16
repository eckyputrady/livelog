{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Applicative          ((<$>), (<*>))
import           Control.Monad.State.Class    (MonadState)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString              as BS
import           Data.Conduit
import qualified Data.Conduit.Binary          as B
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Text            as CT
import qualified Data.Text                    as T
import           Data.Time                    (UTCTime, diffUTCTime,
                                               getCurrentTime)
import           Data.Time                    (NominalDiffTime, TimeZone,
                                               getCurrentTimeZone,
                                               utcToLocalTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Options.Applicative
import           System.Console.Haskeline

import           Database.Esqueleto
import           Database.Persist.MySQL       (withMySQLPool, defaultConnectInfo, connectDatabase)
import           Database.Persist.TH

import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Logger         (LoggingT, runStdoutLoggingT)
import           Control.Monad.Logger         (MonadLogger)

--data Log = Log Tag Message UTCTime deriving (Show, Read)
--type Tag = String
--type Message = String

--data Action = AddLog Tag Message
--            | List Int
--            deriving (Show, Read)

--main :: IO ()
--main = execParser opts >>= process
--  where opts = info (helper <*> commands) (fullDesc <> header "Log your activity")
--        commands = subparser ( logCommand <> listCommand )
--        logCommand = command "log" (info logOptions (progDesc "Log an activity"))
--        logOptions = AddLog <$> (argument str (metavar "TAG")) <*> (argument str (metavar "MESSAGE"))
--        listCommand = command "list" (info listOptions (progDesc "List your N last logged activities"))
--        listOptions = List <$> (argument auto (metavar "N"))

--process :: Action -> IO ()
--process (AddLog tag msg) = do
--  log <- (Log tag msg) <$> getCurrentTime
--  appendFile "db.txt" $ ((show log) <> "\n")

--process (List n) = do
--  logs <- runResourceT $  B.sourceFile "db.txt" $$
--                          B.lines =$=
--                          CT.decode CT.utf8 =$=
--                          CL.map (\x -> (read $ T.unpack x) :: Log) =$
--                          CL.consume
--  curTime <- getCurrentTime
--  mapM_ (printPrettyLog) (withDuration curTime $ lastN n logs)

--withDuration :: UTCTime -> [Log] -> [(Log, NominalDiffTime)]
--withDuration endTime logs =
--  zip logs durations
--  where durations = zipWith diffUTCTime ((tail times) ++ [endTime]) times
--        times = map (\(Log _ _ time) -> time) logs

--printPrettyLog :: (Log, NominalDiffTime) -> IO ()
--printPrettyLog logAndDiff = do
--  timezone <- getCurrentTimeZone
--  putStrLn (prettyLog timezone logAndDiff)

--prettyLog :: TimeZone -> (Log, NominalDiffTime) -> String
--prettyLog timezone ((Log tag msg time), diff) =
--  prettyTime <> " - " <> duration <> "\n  tag: " <> tag <> "\n  " <> msg
--  where prettyTime = fmtTime (utcToLocalTime timezone time)
--        duration = (show $ durationSecs / 60) <> " minutes"
--        durationSecs = diff
--        fmtTime = formatTime defaultTimeLocale "%a %Y/%m/%e %H:%M:%S"

--lastN :: Int -> [a] -> [a]
--lastN n = reverse . take n . reverse

--- DB stuffs

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

--withDB :: MonadIO m => (ConnectionPool -> m ()) -> m ()
withDB f = withMySQLPool connInfo 10 $ \pool -> do
  flip runSqlPool pool $ runMigration migrateAll
  f pool
  where connInfo = defaultConnectInfo { connectDatabase = "livelog" }

-- data CreateLogData = CreateLogData { cldMsg :: String, cldTagName :: String }
-- --createLog :: CreateLogData -> ConnectionPool -> IO ()
-- createLog param pool = flip runSqlPool pool $ do
--   l <- insert $ Log (cldMsg param)
--   t <- insert $ Tag (cldTagName param)
--   insert $ TagLog t l

-- test f = withDB $ \pool -> do
--   return $ flip runSqlPersistMPool pool $ do
--     l <- insert $ Log "test"
--     t <- insert $ Tag "tag"
--     insert $ TagLog t l
--   return ()
--   f

test :: IO ()
test = runStdoutLoggingT . withDB $ \pool -> do
  time <- liftIO getCurrentTime
  logs <- flip runSqlPool pool $ getLogs 10
  liftIO $ putStrLn . show $ logs
  return ()

-- get last n logs
getLogs n = 
  select $ from $ \log -> do
  orderBy [desc (log ^. LogCreatedAt)]
  limit n
  return $ log

-- create log
createLog msg tagName time = do
  logId <- insert $ Log msg time
  tagLog logId tagName

-- edit log msg

-- get tag
getTag tagName = do
  tags <- select $ from $ \tag -> do
          where_ (tag ^. TagName ==. val tagName)
          limit 1
          return tag
  return $ case tags of
    []  -> Nothing
    [x] -> Just x  

-- create tag
createTag tagName = do
  maybeTag <- getTag tagName
  case maybeTag of
    Nothing -> insert $ Tag tagName
    Just v  -> return $ entityKey v

-- edit tag name

-- add log tagging
tagLog logId tagName = do
  tagId <- createTag tagName
  insert $ TagLog tagId logId

-- rem log tagging
