{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text.Read (decimal)

main :: IO ()
main = do
  putStrLn "Введите числа по одному в строке. Введите '=', чтобы получить сумму."
  numbers <- getNumbers []
  putStrLn $ "Сумма чисел: " ++ show (sum numbers)

getNumbers :: [Int] -> IO [Int]
getNumbers acc = do
  line <- TIO.getLine
  if line == "="
    then return acc
    else case decimal line of
      Right (num, _) -> getNumbers (num : acc)
      Left _ -> do
        putStrLn "Некорректный ввод, введите число или '='."
        getNumbers acc
