module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Network.HTTP.Types.Status (statusCode, statusMessage)

noaaHost :: BC.ByteString
noaaHost = "www.ncei.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/access/services/search/v1/datasets"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest host method path =
    setRequestMethod method
    $ setRequestHost host
    $ setRequestPath path
    $ setRequestSecure True
    $ setRequestPort 443
    $ addRequestHeader "Accept-Encoding" (BC.pack "application/json")
    $ defaultRequest

-- Задача 39.1
buildRequestNOSSL :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequestNOSSL host method path =
    setRequestMethod method
    $ setRequestHost host
    $ setRequestPath path
    $ setRequestSecure False
    $ setRequestPort 80
    $ addRequestHeader "Accept-Encoding" (BC.pack "application/json")
    $ defaultRequest

-- Задача 39.2
main :: IO ()
main = do
    let request = buildRequestNOSSL noaaHost "GET" apiPath
    response <- httpLBS request
    let status = getResponseStatus response
        code = statusCode status
        message = statusMessage status
    if code == 200
        then do
            putStrLn "Succesfully saved data to data.json"
            L.writeFile "data.json" (getResponseBody response)
        else
            putStrLn $ "HTTP Error " ++ show code ++ ": " ++ BC.unpack message
