import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read (readMaybe)

type UserInput = Map String String

greetUser :: UserInput -> Maybe String
greetUser userInput = do
  name   <- Map.lookup "name" userInput
  ageStr <- Map.lookup "age" userInput
  age    <- readMaybe ageStr :: Maybe Int
  city   <- Map.lookup "city" userInput
  return $ "Привет, " ++ name ++ "! Тебе " ++ show age ++ " лет, и ты из " ++ city ++ "."


main :: IO ()
main = do
  let input = Map.fromList [("name", "Иван"), ("age", "25"), ("city", "Москва")]
  case greetUser input of
    Just greeting -> putStrLn greeting
    Nothing       -> putStrLn "Не удалось сформировать приветствие."
