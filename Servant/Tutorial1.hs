-- -----------------------------------------------------------------
-- A first simple Servant Example
-- ----------------------------------------------------------------
--
-- This corresponds to tutorial 1 and 2
--
-- See:
--    https://haskell-servant.github.io/tutorial/server
--
-- ----------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API

-- -------------------------------------------------------------
-- Define the datatype for our API
-- -------------------------------------------------------------
data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registrationDate :: String        -- Here should be a Day datatype
  } deriving (Eq, Show, Generic)

instance ToJSON User

-- | Define our simple API
type UserAPI2 = "users" :> Get '[JSON] [User]
             :<|> "albert" :> Get '[JSON] User
             :<|> "isaac"  :> Get '[JSON] User

-- | Create the server SAME SEQUENCE as in the API definition
myServer :: Server UserAPI2
myServer = return users
         :<|> return albert
         :<|> return isaac

-- | Our endpoint handlers
users :: [User]
users = userList

isaac :: User
isaac = head userList

albert :: User
albert = last userList

-- --------------------------------------------------------
-- Boilerplate for plumbing up this to a WebServer
-- --------------------------------------------------------
userAPI :: Proxy UserAPI2
userAPI = Proxy

app1 :: Application
app1 = serve userAPI myServer

main :: IO()
main = run 8081 app1

-- -----------------------------------------------------------
-- Some Test data
-- -----------------------------------------------------------
userList :: [User]
userList =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" "1683  3 1"
  , User "Albert Einstein" 136 "ae@mc2.org"         "1905 12 1"
  ]
