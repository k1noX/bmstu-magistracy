module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import GHC.Generics
import Control.Monad (forM_)
import Data.List (intercalate)
import qualified Data.Text as T

data NOAASearchResponse = NOAASearchResponse
  { totalCount :: Int
  , results    :: [Dataset]
  } deriving (Show, Generic)

instance FromJSON NOAASearchResponse where
  parseJSON (Object v) =
    NOAASearchResponse
      <$> v .: "totalCount"
      <*> v .: "results"
  parseJSON _ = fail "Expected object"

data Dataset = Dataset
  { datasetId   :: T.Text
  , name        :: T.Text
  , description :: T.Text
  , startDate   :: T.Text
  , endDate     :: T.Text
  , available   :: Bool
  , keywords    :: Maybe [Keyword]
  , links       :: Maybe AccessLinks
  } deriving (Show, Generic)

instance FromJSON Dataset where
  parseJSON (Object v) =
    Dataset
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "description"
      <*> v .: "startDate"
      <*> v .: "endDate"
      <*> v .: "available"
      <*> v .:? "keywords"
      <*> v .:? "links"
  parseJSON _ = fail "Expected object"

data Keyword = Keyword
  { keywordName :: T.Text
  , keywordID   :: T.Text
  } deriving (Show, Generic)

instance FromJSON Keyword where
  parseJSON (Object v) =
    Keyword
      <$> v .: "name"
      <*> v .: "id"
  parseJSON _ = fail "Expected Keyword object"

data AccessLinks = AccessLinks
  { other :: Maybe [Link]
  , access :: Maybe [Link]
  , documentation :: Maybe [Link]
  } deriving (Show, Generic)

instance FromJSON AccessLinks where
  parseJSON (Object v) =
    AccessLinks
      <$> v .:? "other"
      <*> v .:? "access"
      <*> v .:? "documentation"
  parseJSON _ = fail "Expected object for links"

data Link = Link
  { linkName :: T.Text
  , linkType :: T.Text
  , linkURL  :: T.Text
  } deriving (Show, Generic)

instance FromJSON Link where
  parseJSON (Object v) =
    Link
      <$> v .: "name"
      <*> v .: "type"
      <*> v .: "url"
  parseJSON _ = fail "Expected object"

printResults :: Maybe [Dataset] -> IO ()
printResults Nothing = putStrLn "Error: failed to load datasets."
printResults (Just datasets) = do
  putStrLn $ "Found datasets: " ++ show (length datasets)
  forM_ datasets $ \ds -> do
    putStrLn $ "* " ++ T.unpack (name ds)
    putStrLn $ "  ID: " ++ T.unpack (datasetId ds)
    putStrLn $ "  Period: " ++ T.unpack (startDate ds) ++ " - " ++ T.unpack (endDate ds)
    putStrLn $ "  Available: " ++ if available ds then "Yes" else "No"
    putStrLn $ "  Description: " ++ T.unpack (description ds)

    case keywords ds of
      Nothing -> return ()
      Just kws -> do
        putStr "  Keywords: "
        let kwStrs = map (\kw -> T.unpack (keywordName kw)) kws
        putStrLn (intercalate ", " kwStrs)

    case links ds of
      Nothing -> return ()
      Just al -> case access al of
        Nothing -> return ()
        Just linkList -> do
          putStrLn "  Access URLs:"
          forM_ linkList $ \lnk -> do
            putStrLn $ "    - " ++ T.unpack (linkURL lnk)

    putStrLn ""

-- Задача 40.1
instance ToJSON NOAASearchResponse where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' }

instance ToJSON Dataset where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' }

instance ToJSON Keyword where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = \s ->
        case s of
          "keywordName" -> "name"
          "keywordID"   -> "id"
          _             -> s
    }

instance ToJSON AccessLinks where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' }


instance ToJSON Link where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = \s ->
        case s of
          "linkName" -> "name"
          "linkType" -> "type"
          "linkURL"  -> "url"
          _          -> s
    }

main :: IO ()
main = do
  result <- eitherDecodeFileStrict "data.json" :: IO (Either String NOAASearchResponse)
  case result of
    Left err -> putStrLn $ "Ошибка парсинга JSON: " ++ err
    Right response -> do
        printResults (Just (results response))
        L.writeFile "reformatted.json" (encode response)

-- Задача 40.2
data IntList = EmptyList | Cons Int (IntList) deriving (Show, Generic)

intListExample :: IntList
intListExample = Cons 1 $ Cons 2 EmptyList

instance ToJSON IntList
