import System.Environment (getArgs)
import qualified Data.ByteString as BS
import System.Random (randomRIO)
import Data.List (sortBy)
import Data.Ord (comparing)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> randomReverseBytes fileName
    _ -> putStrLn "Использование: random_reverse_bytes <имя_файла>"

randomReverseBytes :: FilePath -> IO ()
randomReverseBytes fileName = do
  content <- BS.readFile fileName
  let len = BS.length content
  if len < 2
    then putStrLn "Файл слишком мал для реверса подпоследовательности."
    else do
      start <- randomRIO (0, len - 2)
      end   <- randomRIO (start + 1, len - 1)
      let reversedContent = reverseSubBytes content start end
      BS.writeFile fileName reversedContent
      putStrLn $ "Реверс байтов с " ++ show start ++ " по " ++ show end ++ " выполнен."

reverseSubBytes :: BS.ByteString -> Int -> Int -> BS.ByteString
reverseSubBytes bs start end =
  let (before, rest) = BS.splitAt start bs
      (sub, after) = BS.splitAt (end - start + 1) rest
  in BS.concat [before, BS.reverse sub, after]