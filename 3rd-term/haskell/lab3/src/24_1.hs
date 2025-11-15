import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src, dest] -> copyFile' src dest
    _ -> putStrLn "Использование: mycp <исходный_файл> <новый_файл>"

copyFile' :: FilePath -> FilePath -> IO ()
copyFile' src dest = do
  content <- B.readFile src
  B.writeFile dest content
