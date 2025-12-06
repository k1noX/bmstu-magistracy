module Book where

import           Data.Aeson
import           Data.ByteString.Lazy          as B
import           Data.ByteString.Lazy.Char8    as BC
import           Data.Text                     as T
import           GHC.Generics

data Book = Book
  { title  :: T.Text
  , author :: T.Text
  , year   :: Int
  } deriving (Show, Generic)

instance FromJSON Book

instance ToJSON Book

myBook = Book {title = "Will Kurt", author = "Learn Haskell", year = 2017}

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

e1 :: Maybe Book
e1 = decode myBookJSON

rawJSON :: BC.ByteString
rawJSON =
  "{\"year\":1949,\"author\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\"}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

wrongJSON :: BC.ByteString
wrongJSON =
  "{\"year\":1949,\"writer\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\"}"

bookFromWrongJSON = decode wrongJSON :: Maybe Book

bookFromWrongJSON' = eitherDecode wrongJSON :: Either String Book

data Name = Name
  { firstName :: T.Text
  , lastName :: T.Text
  } deriving (Show, Generic)

instance FromJSON Name
instance ToJSON Name
