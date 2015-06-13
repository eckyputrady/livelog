{-# LANGUAGE OverloadedStrings #-}

import Data.Time (UTCTime, getCurrentTime)
import Control.Monad.State.Class (MonadState)
import Control.Applicative ((<$>), (<*>))
import System.Console.Haskeline
import Options.Applicative
import Data.Conduit
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.List as CL

data Log = Log Tag Message UTCTime deriving (Show, Read)
type Tag = String
type Message = String

data Action = AddLog Tag Message
            | List Int
            deriving (Show, Read)

main :: IO ()
main = execParser opts >>= process
  where opts = info (helper <*> commands) (fullDesc <> header "Log your activity")
        commands = subparser ( logCommand <> listCommand )
        logCommand = command "log" (info logOptions (progDesc "Log an activity"))
        logOptions = AddLog <$> (argument str (metavar "TAG")) <*> (argument str (metavar "MESSAGE"))
        listCommand = command "list" (info listOptions (progDesc "List your N last logged activities"))
        listOptions = List <$> (argument auto (metavar "N"))

process :: Action -> IO ()
process (AddLog tag msg) = do
  log <- (Log tag msg) <$> getCurrentTime
  appendFile "db.txt" $ ((show log) <> "\n")

process (List n) = do
  logs <- runResourceT $  B.sourceFile "db.txt" $$
                          B.lines =$=
                          CT.decode CT.utf8 =$=
                          CL.map (\x -> (read $ T.unpack x) :: Log) =$
                          CL.consume
  mapM_ (putStrLn . show) (lastN n logs)

lastN :: Int -> [a] -> [a]
lastN n = reverse . take n . reverse

--mkLog :: Tag -> Message -> IO Log
--mkLog tag msg = (Log tag msg) <$> getCurrentTime

--addLog :: Log -> [Log] -> [Log]
--addLog = (:)

--main :: IO ()
--main = runInputT defaultSettings $ loop []

--loop :: [Log] -> InputT IO ()
--loop appState = do
--  minput <- getInputLine "> "
--  case parseInput minput of
--    Exit    -> return ()
--    action  -> loop =<< exec action appState

--parseInput :: Maybe String -> Action
--parseInput Nothing = NoOp
--parseInput (Just ('e':'x':'i':'t':xs)) = Exit
--parseInput (Just ('l':'o':'g':xs)) = AddLog
--parseInput _ = Unknown

--exec :: Action -> [Log] -> InputT IO [Log]
--exec _ appState = do
--  outputStrLn "exec something"
--  return appState
