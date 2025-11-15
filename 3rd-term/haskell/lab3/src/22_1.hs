import Text.Read (readMaybe)

calculate :: String -> Maybe Double
calculate input = case words input of
  [num1Str, "+", num2Str] -> do
    num1 <- readMaybe num1Str
    num2 <- readMaybe num2Str
    return (num1 + num2)
  [num1Str, "*", num2Str] -> do
    num1 <- readMaybe num1Str
    num2 <- readMaybe num2Str
    return (num1 * num2)
  _ -> Nothing

main :: IO ()
main = do
  putStrLn "Введите выражение."
  putStrLn "Для выхода используйте Ctrl+C."
  loop

loop :: IO ()
loop = do
  putStr "> "
  input <- getLine
  case calculate input of
    Just result -> print result
    Nothing     -> putStrLn "Неверный формат выражения."
  loop
