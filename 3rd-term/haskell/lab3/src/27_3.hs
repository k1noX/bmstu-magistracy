import Data.Map (Map)
import qualified Data.Map as Map

type Cost = Double

type PartsDB = Map String Cost

partsDB :: PartsDB
partsDB = Map.fromList
  [ ("R001", 25.99)
  , ("R002", 15.50)
  , ("R003", 42.75)
  , ("R004", 10.20)
  ]

lookupCost :: String -> PartsDB -> Maybe Cost
lookupCost = Map.lookup

main :: IO ()
main = do
  putStrLn "Введите идентификатор компонента (или ':q' для выхода):"
  input <- getLine
  if input == ":q"
    then putStrLn "\n"
    else do
      let result = lookupCost input partsDB
      case result of
        Nothing -> putStrLn $ "Компонент с идентификатором '" ++ input ++ "' не найден."
        Just cost -> putStrLn $ "Стоимость компонента: " ++ show cost ++ " у.е."
      main