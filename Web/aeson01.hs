-- ---------------------------------------------------------------------------
-- aeson01.hs Combine ason00.hs with wreq01.hs
-- ---------------------------------------------------------------------------
-- 
-- Fetch a a JSON string from the Net with the wreq library and parse it 
--    to a JSON value
-- ---------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Control.Applicative( (<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BS

import Network.Wreq
import Control.Lens

-- | 1. Define a datatype for the json data
data Paste = Paste { getLines    :: Integer
                   , getDate     :: String
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
          <*> (v .: "date")
          <*> (v .: "paste-id")
          <*> (v .: "language")
          <*> (v .: "private")
          <*> (v .: "url")
          <*> (v .:? "user")
          <*> (v .: "contents")

-- | 3. Fetch the a JSON string from the NET and decode it
getJson :: IO (Maybe Paste)
getJson = do
   rsp <- get "https://www.refheap.com/api/paste/2"
   let body = rsp ^. responseBody
   return $ decode body

-- | 4. TODO: Add some reasonable error handling
--    eg: Check the status code before decoding the body

