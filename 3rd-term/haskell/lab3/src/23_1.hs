{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

helloPerson :: T.Text -> T.Text
helloPerson name = "Привет, " <> name <> "!"

main :: IO ()
main = do
  TIO.putStr "Введите ваше имя: "
  name <- TIO.getLine
  let statement = helloPerson name
  TIO.putStrLn statement
