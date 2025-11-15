import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data RobotPart = RobotPart
  { name :: String
  , description :: String
  , cost :: Double
  , count :: Int
  } deriving Show

partsDB :: Map String RobotPart
partsDB = Map.fromList
  [ ("R001", RobotPart "Motor" "High-torque motor" 45.99 10)
  , ("R002", RobotPart "Wheel" "Rubber wheel" 12.50 50)
  , ("R003", RobotPart "Sensor" "Proximity sensor" 28.75 20)
  , ("R004", RobotPart "Battery" "Rechargeable battery" 35.00 15)
  , ("R005", RobotPart "Controller" "Main controller" 120.00 5)
  ]

getPart :: String -> Maybe RobotPart
getPart partId = Map.lookup partId partsDB

cheaperPart :: RobotPart -> RobotPart -> RobotPart
cheaperPart p1 p2 = if cost p1 <= cost p2 then p1 else p2

getPartOrFail :: String -> IO (Maybe RobotPart)
getPartOrFail partId = return $ getPart partId

main :: IO ()
main = do
  putStrLn "Введите ID первой части:"
  id1 <- getLine
  putStrLn "Введите ID второй части:"
  id2 <- getLine

  let part1 = getPart id1
  let part2 = getPart id2

  case (part1, part2) of
    (Nothing, Nothing) -> putStrLn $ "Обе части с ID '" ++ id1 ++ "' и '" ++ id2 ++ "' не найдены."
    (Nothing, _) -> putStrLn $ "Часть с ID '" ++ id1 ++ "' не найдена."
    (_, Nothing) -> putStrLn $ "Часть с ID '" ++ id2 ++ "' не найдена."
    (Just p1, Just p2) -> do
      let cheaper = cheaperPart p1 p2
      putStrLn $ "Часть с меньшей ценой: " ++ show cheaper
