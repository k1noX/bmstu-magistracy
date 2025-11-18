import System.Environment (getArgs)
import System.IO (withBinaryFile, IOMode(ReadMode), hFileSize)
import qualified Data.ByteString as BS

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> compareFileSize fileName
    _ -> putStrLn "Использование: compare_size <имя_файла>"

compareFileSize :: FilePath -> IO ()
compareFileSize fileName = do
  content <- readFile fileName
  let charCount = length content

  fileSize <- withBinaryFile fileName ReadMode hFileSize

  putStrLn $ "Количество символов: " ++ show charCount
  putStrLn $ "Размер файла в байтах: " ++ show fileSize
  putStrLn $ if fromIntegral charCount == fileSize
             then "Количество символов совпадает с размером в байтах (файл, вероятно, ASCII)."
             else "Количество символов не совпадает с размером в байтах (файл, вероятно, содержит не-ASCII символы)."
