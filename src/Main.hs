
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad.State.Class (MonadState)
import Control.Applicative ((<$>), (<*>))
import System.Console.Haskeline
import Options.Applicative

data Log = Log Tag Message UTCTime deriving (Show)
type Tag = String
type Message = String

data Action = AddLog Tag Message
            | List Integer
            deriving (Show)

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
  putStrLn ("logged " <> show log)

process (List n) = do
  putStrLn "showing n last"

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
