module Main where

import Control.Applicative
import Data.Time
import Data.Time.Calendar (dayOfWeek)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import System.IO

connInfo :: ConnectInfo
connInfo = ConnectInfo
  { connectHost     = "localhost"
  , connectPort     = 5432
  , connectUser     = "myuser"
  , connectPassword = "mypassword"
  , connectDatabase = "mydatabase"
  }

data Tool = Tool
  { toolId        :: Int
  , name          :: String
  , description   :: String
  , lastReturned  :: Maybe Day
  , timesBorrowed :: Int
  }

instance Show Tool where
  show tool = mconcat
    [ show (toolId tool)
    , ".) "
    , name tool
    , "\n description: "
    , description tool
    , "\n last returned: "
    , show (lastReturned tool)
    , "\n times borrowed: "
    , show (timesBorrowed tool)
    ]

data User = User
  { userId   :: Int
  , userName :: String
  }

instance Show User where
  show user = mconcat [show (userId user), ".) ", userName user]

withConn :: (Connection -> IO ()) -> IO ()
withConn action = do
  conn <- connect connInfo
  action conn
  close conn

addUser :: String -> IO ()
addUser userName = withConn $ \conn -> do
  execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)
  putStrLn "User added."

-- Задание 41.1
addTool :: String -> String -> IO ()
addTool name description = withConn $ \conn -> do
  execute conn
    "INSERT INTO tools (name, description, timesBorrowed) VALUES (?, ?, ?)"
    (name, description, 0 :: Int)
  putStrLn "Tool added."

checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn $ \conn ->
  execute conn
    "INSERT INTO checkedout (user_id, tool_id) VALUES (?, ?)"
    (userId, toolId)

instance FromRow User where
  fromRow = User <$> field <*> field

instance FromRow Tool where
  fromRow = Tool <$> field <*> field <*> field <*> field <*> field

printUsers :: IO ()
printUsers = withConn $ \conn -> do
  response <- query_ conn "SELECT * FROM users" :: IO [User]
  mapM_ print response

printToolQuery :: Query -> IO ()
printToolQuery q = withConn $ \conn -> do
  response <- query_ conn q :: IO [Tool]
  mapM_ print response

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools"

printAvailableTools :: IO ()
printAvailableTools =
  printToolQuery "SELECT * FROM tools WHERE id NOT IN (SELECT tool_id FROM checkedout)"

printCheckedoutTools :: IO ()
printCheckedoutTools =
  printToolQuery "SELECT * FROM tools WHERE id IN (SELECT tool_id FROM checkedout)"

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  response <- query conn "SELECT * FROM tools WHERE id = ?" (Only toolId) :: IO [Tool]
  return $ case response of
    []      -> Nothing
    (x:_)   -> Just x

updateTool :: Tool -> Day -> Tool
updateTool tool date =
  tool { lastReturned = Just date, timesBorrowed = 1 + timesBorrowed tool }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = putStrLn "ID not found"
updateOrWarn (Just tool) = withConn $ \conn -> do
  execute conn
    "UPDATE tools SET lastReturned = ?, timesBorrowed = ? WHERE id = ?"
    (lastReturned tool, timesBorrowed tool, toolId tool)
  putStrLn "Tool updated"

updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn $ \conn -> do
  tool <- selectTool conn toolId
  currentDay <- utctDay <$> getCurrentTime
  case tool of
    Nothing -> putStrLn "ID not found"
    Just t  -> do
      let updated = updateTool t currentDay
      execute conn
        "UPDATE tools SET lastReturned = ?, timesBorrowed = ? WHERE id = ?"
        (lastReturned updated, timesBorrowed updated, toolId updated)
      putStrLn "Tool updated"

checkin :: Int -> IO ()
checkin toolId = withConn $ \conn ->
  execute conn "DELETE FROM checkedout WHERE tool_id = ?" (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
  checkin toolId
  updateToolTable toolId

promptAndAddUser :: IO ()
promptAndAddUser = do
  hSetBuffering stdout NoBuffering
  putStr "User name? "
  hFlush stdout
  userName <- getLine
  addUser userName

-- Задание 41.2
promptAndAddTool :: IO ()
promptAndAddTool = do
  hSetBuffering stdout NoBuffering
  putStr "Tool name? "
  hFlush stdout
  name <- getLine
  putStr "Tool description? "
  hFlush stdout
  description <- getLine
  addTool name description

promptAndCheckout :: IO ()
promptAndCheckout = do
  hSetBuffering stdout NoBuffering
  putStr "User ID? "
  hFlush stdout
  userId <- read <$> getLine
  putStr "Tool ID? "
  hFlush stdout
  toolId <- read <$> getLine
  checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
  hSetBuffering stdout NoBuffering
  putStr "Tool ID? "
  hFlush stdout
  toolId <- read <$> getLine
  checkinAndUpdate toolId

performCommand :: String -> IO ()
performCommand "users"    = printUsers >> main
performCommand "tools"    = printTools >> main
performCommand "adduser"  = promptAndAddUser >> main
performCommand "addtool"  = promptAndAddTool >> main
performCommand "checkout" = promptAndCheckout >> main
performCommand "checkin"  = promptAndCheckin >> main
performCommand "in"       = printAvailableTools >> main
performCommand "out"      = printCheckedoutTools >> main
performCommand "quit"     = putStrLn "Bye!"
performCommand _          = putStrLn "Error: command not found" >> main

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Command? "
  hFlush stdout
  command <- getLine
  performCommand command
