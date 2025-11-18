import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (toUpper)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> capitalizeFile fileName
    _ -> putStrLn "Использование: capitalize <имя_файла>"

capitalizeFile :: FilePath -> IO ()
capitalizeFile fileName = do
  content <- TIO.readFile fileName
  let upperContent = T.map toUpper content
  TIO.writeFile fileName upperContent
