-- ----------------------------------------------------------------------------
-- First examples with aeson
-- ----------------------------------------------------------------------------
--
-- See: http://blog.raynes.me/blog/2012/11/27/easy-json-parsing-in-haskell-with-aeson/
--
-- This example shows, how to parse a json string into a corresponding
--     Haskell type
-- ----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Control.Applicative( (<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BS

-- Imports needed to parse the UTC time
import Data.Time.Format     (parseTime)
import Data.Time.Clock      (UTCTime)
import System.Locale        (defaultTimeLocale)
import Control.Monad        (liftM)

-- This is the output from a webservice returning a JSON string:
{-

$ http https://www.refheap.com/api/paste/1
HTTP/1.1 200 OK
Connection: keep-alive
Content-Length: 226
Content-Type: application/json;charset=utf-8
Date: Tue, 27 Nov 2012 08:39:26 GMT
Server: Jetty(7.6.1.v20120215)

{
    "contents": "(begin)", 
    "date": "2012-01-04T01:44:22.964Z", 
    "fork": null, 
    "language": "Clojure", 
    "lines": 1, 
    "paste-id": "1", 
    "private": false, 
    "random-id": "f1fc1181fb294950ca4df7008", 
    "url": "https://www.refheap.com/paste/1", 
    "user": "raynes"
}

-}

-- | 1. Define a datatype for the json data
data Paste = Paste { getLines    :: Integer
                   , getDate     :: Maybe UTCTime
                   , getID       :: String
                   , getLanguage :: String
                   , getPrivate  :: Bool
                   , getURL      :: String
                   , getUser     :: Maybe String
                   , getBody     :: String
                   } deriving (Show)

-- | 2. Define the datatype as an instance of FromJSON
instance FromJSON Paste where
  parseJSON (Object v) =
    Paste <$> (v .: "lines")
          <*> liftM parseRHTime (v .: "date")
          <*> (v .: "paste-id")
          <*> (v .: "language")
          <*> (v .: "private")
          <*> (v .: "url")
          <*> (v .:? "user")
          <*> (v .: "contents")

-- | Function to parse and UTCTime
parseRHTime :: String -> Maybe UTCTime
parseRHTime = parseTime defaultTimeLocale "%FT%X%QZ"

-- An example json string from the blog
-- >>> let json = BS.pack "{\"lines\":1,\"date\":\"2012-01-04T01:44:22.964Z\",\"paste-id\":\"1\",\"fork\":null,\"random-id\":\"f1fc1181fb294950ca4df7008\",\"language\":\"Clojure\",\"private\":false,\"url\":\"https://www.refheap.com/paste/1\",\"user\":\"raynes\",\"contents\":\"(begin)\"}"

-- Look. whether we are abele to decode
-- Note we have to specify the resulting type
-- >>>  decode json :: Maybe Paste 

-- Ok, bind the resuult to the name x
-- >>> let (Just x) = decode json :: Maybe Paste

-- Extract a value
-- >>> getBody x

-- A similar json string fetched with :
--  http https://www.refheap.com/api/paste/2

{-

{
    "contents": "(second \"paste\")", 
    "date": "2012-01-04T01:45:12.363Z", 
    "language": "Clojure", 
    "lines": 1, 
    "paste-id": "2", 
    "private": false, 
    "url": "https://www.refheap.com/2", 
    "user": "amcnamara", 
    "views": 44
}

-}

-- Replace " by \"
{-

{
    \"contents\": \"(second paste)\", 
    \"date\": \"2012-01-04T01:45:12.363Z\", 
    \"language\": \"Clojure\", 
    \"lines\": 1, 
    \"paste-id\": \"2\", 
    \"private\": false, 
    \"url\": \"https://www.refheap.com/2\", 
    \"user\": \"amcnamara\", 
    \"views\": 44
}

-}

-- Enclose in "" and remove line ends

-- >>> let myjson = BS.pack "{\"contents\": \"(second paste)\", \"date\": \"2012-01-04T01:45:12.363Z\", \"language\": \"Clojure\", \"lines\": 1, \"paste-id\": \"2\", \"private\": false, \"url\": \"https://www.refheap.com/2\", \"user\": \"amcnamara\", \"views\": 44}"
-- >>> decode myjson :: Maybe Paste

