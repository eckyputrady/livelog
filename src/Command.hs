{-# LANGUAGE OverloadedStrings          #-}

module Command where

import           Control.Applicative          ((<$>), (<*>))
import           Options.Applicative
import Data.Time (UTCTime, getCurrentTime)
import Data.Text (Text)

type Event = (UTCTime, Command)

data Command  = CreateLog String String -- tag & message
              | TagLog Integer String
              | ListLog Integer
              deriving (Show)

-- parseCommand :: String -> IO Command
-- parseCommand str = handleParseResult $ execParserPure default _ _
--   where opts        = info (helper <*> commands) (fullDesc <> header "Log your activity")           
--         commands    = subparser ( logCommand <> listCommand )
--         logCommand  = command "log" (info logOptions (progDesc "Log an activity"))
--         logOptions  = CreateLog <$> (argument str (metavar "TAG")) <*> (argument str (metavar "MESSAGE"))
--         listCommand = command "list" (info listOptions (progDesc "List your N last logged activities"))
--         listOptions = ListLog <$> (argument auto (metavar "N"))