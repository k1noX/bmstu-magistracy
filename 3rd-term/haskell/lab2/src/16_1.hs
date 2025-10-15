type FirstName = String
type LastName = String
type MiddleName = String
data Name = Name FirstName LastName
    | NameWithMiddle FirstName MiddleName LastName
    | TwoInitialsWithLast Char Char LastName
    | FirstNameWithInit FirstName Char

data Creator = AuthorCreator Author | ArtistCreator Artist
data Author = Author Name
data Artist = Person Name | Band String

data Book = Book 
    { author :: Creator
    , isbn :: String
    , bookTitle :: String
    , bookYear :: Int
    , bookPrice :: Double
    }

data VinylRecord = VinylRecord
    { artist :: Creator
    , recordTitle :: String
    , recordYear  :: Int
    , recordPrice :: Double
    }

data Toy = Toy
  { toyName :: String
  , toyDesc :: String
  , toyPrice :: Double
  } deriving (Show)

-- Pamphlet - бесплатная брошюра в магазине
data Pamphlet = Pamphlet
  { pamphletTitle :: String
  , pamphletDesc  :: String
  , contactInfo   :: String
  } deriving (Show)

data StoreItem = BookItem Book
    | RecordItem VinylRecord
    | ToyItem Toy 
    | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (ToyItem toy) = toyPrice toy
price (RecordItem record) = recordPrice record
price (PamphletItem _) = 0.0
