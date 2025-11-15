{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Text.Encoding.Error (OnDecodeError, strictDecode, UnicodeException)
import Data.Maybe
import System.Environment (getArgs)
import Data.Char (chr, ord)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      marcData <- B.readFile inputFile
      let processed = processRecords 500 marcData
      -- Пришлось фильтровать, потому что где-то попадается не-UTF символ в имени книги
      let safeProcessed = T.filter (\c -> ord c /= 65533) processed
      TIO.writeFile outputFile safeProcessed
      putStrLn $ "Файл " ++ outputFile ++ " успешно создан."
    _ -> putStrLn "Использование: 26.exe <входной.mrc> <выходной.html>"

type Author = T.Text
type Title = T.Text

data Book = Book {
  author :: Author
 ,title :: Title } deriving Show

type Html = T.Text

bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n"
                     ,titleInTags
                     ,authorInTags
                     ,"</p>\n"]
  where titleInTags = mconcat ["<strong>", title book, "</strong>\n"]
        authorInTags = mconcat ["<em>", author book, "</em>\n"]

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat ["<html>\n"
                            ,"<head><title>books</title>"
                            ,"<meta charset='utf-8'/>"
                            ,"</head>\n"
                            ,"<body>\n"
                            ,booksHtml
                            ,"\n</body>\n"
                            ,"</html>"]
  where booksHtml = (mconcat . map bookToHtml) books
type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString

leaderLength :: Int
leaderLength = 24

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader = B.take leaderLength

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
  where recordLength = getRecordLength marcStream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream = if marcStream == B.empty
                        then []
                        else next : allRecords rest
  where (next, rest) = nextAndRest marcStream

type MarcDirectoryRaw = B.ByteString

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 remainder)
  where remainder = B.drop 12 leader

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
  where directoryLength = getDirectoryLength record
        afterLeader = B.drop leaderLength record

type MarcDirectoryEntryRaw = B.ByteString

dirEntryLength :: Int
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory = if directory == B.empty
                           then []
                           else nextEntry : splitDirectory restEntries
  where (nextEntry, restEntries) = B.splitAt dirEntryLength directory

data FieldMetadata = FieldMetadata {tag :: T.Text
                                   ,fieldLength :: Int
                                   ,fieldStart :: Int } deriving Show

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata textTag theLength theStart
  where (theTag, rest) = B.splitAt 3 entry
        textTag = E.decodeUtf8 theTag
        (rawLength, rawStart) = B.splitAt 4 rest
        theLength = rawToInt rawLength
        theStart = rawToInt rawStart

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata = map makeFieldMetadata

type FieldText = T.Text

getCharacterCodingScheme :: MarcLeaderRaw -> Char
getCharacterCodingScheme leader = chr $ fromEnum $ B.index leader 9

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = decodeBasedOnCoding record byteStringValue
  where recordLength = getRecordLength record
        baseAddress = getBaseAddress record
        baseRecord = B.drop baseAddress record
        baseAtEntry = B.drop (fieldStart fieldMetadata) baseRecord
        byteStringValue = B.take (fieldLength fieldMetadata) baseAtEntry

skipDecodeError :: OnDecodeError
skipDecodeError x y = Nothing

decodeBasedOnCoding :: MarcRecordRaw -> B.ByteString -> FieldText
decodeBasedOnCoding record raw = 
  if codingScheme == 'a' || codingScheme == ' '
    then E.decodeUtf8 raw
    else E.decodeUtf8With skipDecodeError raw
  where codingScheme = getCharacterCodingScheme record

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfieldA :: Char
titleSubfieldA = 'a'

titleSubfieldB :: Char
titleSubfieldB = 'b'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record = if null results
                                  then Nothing
                                  else Just (head results)
  where metadata = (getFieldMetadata . splitDirectory . getDirectory) record
        results = filter ((== aTag) . tag) metadata

lookupAllSubfields :: Maybe FieldMetadata -> Char -> MarcRecordRaw -> [T.Text]
lookupAllSubfields Nothing subfield record = []
lookupAllSubfields (Just fieldMetadata) subfield record =
  let rawField = getTextField record fieldMetadata
      subfields = T.split (== fieldDelimiter) rawField
  in map (T.drop 1) $ filter ((== subfield) . T.head) subfields

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle record = do
  fieldMetadata <- lookupFieldMetadata titleTag record
  let aSubfields = lookupAllSubfields (Just fieldMetadata) titleSubfieldA record
      bSubfields = lookupAllSubfields (Just fieldMetadata) titleSubfieldB record
  if null aSubfields
    then Nothing
    else Just $ T.intercalate " " (aSubfields ++ bSubfields)

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor record = do
  fieldMetadata <- lookupFieldMetadata authorTag record
  let subfields = lookupAllSubfields (Just fieldMetadata) authorSubfield record
  if null subfields
    then Nothing
    else Just $ head subfields

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
  where records = allRecords marcStream
        titles = map lookupTitle records
        authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs = map (\(title, author) -> Book {
                            title = fromJust title
                           ,author = fromJust author }) justPairs
  where justPairs = filter (\(title, author) -> isJust title
                                                && isJust author) pairs

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . take n . marcToPairs
