-- Быстрая итеративная версия вычисления n-го числа Фибоначчи
fib :: Integer -> Integer
fib n
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fibIter 0 1 2
  where
    fibIter a b i
      | i > n     = b
      | otherwise = fibIter b (a + b) (i + 1)

main :: IO ()
main = do
  putStr "Введите номер числа Фибоначчи: "
  input <- getLine
  let n = read input :: Integer
  if n >= 0
    then putStrLn $ "F(" ++ show n ++ ") = " ++ show (fib n)
    else putStrLn "Пожалуйста, введите неотрицательное число."