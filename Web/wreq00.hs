-- ----------------------------------------------------------------
-- wreq00.hs - A first look at wreq: a Haskell web client library
-- ----------------------------------------------------------------
--
-- See: [wreq: a Haskell web client library](http://www.serpentine.com/wreq/)
--
-- ----------------------------------------------------------------
{-# Language OverloadedStrings #-}

import Network.Wreq
import Control.Lens

import Data.Map as Map
import Data.Aeson (Value)

-- -----------------------------------------------------------------
-- get
-- -----------------------------------------------------------------

-- | Do a http GET request and extract the whole response status
myget01 = do
   rsp <- get "http://httpbin.org/get"
   return $  rsp ^. responseStatus 

-- | Do a http GET request and extract only the status code
myget02 = do
   rsp <- get "http://httpbin.org/get"
   return $ rsp ^. responseStatus . statusCode

-- | Do a http GET request and extract the html body
myget03 = do
   rsp <- get "https://www.refheap.com/api/paste/2"
   return $  rsp ^. responseBody 
   -- Here we get back the json part of the response !!

-- | Create a Map (Dictionary) of all the fields in the response
type Resp = IO (Response (Map String Value))
myget04 = do
   r <- asJSON =<< get "http://httpbin.org/get" :: Resp
   return r

-- | Format a 
ppField :: (String, Value) -> String
ppField (k,v) = k ++ " => " ++ (show v)

-- | Print out all the response fields nicely
myget05 = do
   r <- asJSON =<< get "http://httpbin.org/get" :: Resp
   -- let hdr  = r ^. responseHeader
   -- mapM putStrLn (Prelude.map ppField (toList r))
   mapM_ putStrLn (Prelude.map ppField (Map.toList(r ^. responseBody))) 


