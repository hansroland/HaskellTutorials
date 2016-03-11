-- -----------------------------------------------------------------
-- A Servant Example with arguments
-- ----------------------------------------------------------------
--
-- This corresponds to tutorial 3
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

import Control.Monad.Trans.Either
import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API

import Data.List(intercalate)
import DemoTypes

-- --------------------------------------------------------------------
-- Data Types
-- -------------------------------------------------------------------

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'

  where from'    = "great@company.com"
        to'      = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body'    = "Hi " ++ clientName c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (clientAge c)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (clientInterestedIn c)
                ++ " products? Give us a visit!"

-- | API definition
type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

-- | Handler definitons
server3 :: Server API
server3 = position
      :<|> hello
      :<|> marketing

-- The position handler
position :: Int -> Int -> EitherT ServantErr IO Position
position x y = return (Position x y)

hello :: Maybe String -> EitherT ServantErr IO HelloMessage
hello mname = return . HelloMessage $ case mname of
    Nothing -> "Hello, anonymous coward"
    Just n  -> "Hello, " ++ n

marketing :: ClientInfo -> EitherT ServantErr IO Email
marketing clientinfo = return (emailForClient clientinfo)

-- --------------------------------------------------------
-- Boilerplate for plumbing up this to a WebServer
-- --------------------------------------------------------
userAPI :: Proxy API
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server3

main :: IO()
main = run 8081 app1

-- How to call
-- http://localhost:8081/position/15/16
-- http://localhost:8081/hello?name=Roland
-- curl -X POST -d '{"name":"Alp Mestanogullari", "email" : "alp@foo.com", "age": 25, "interested_in": ["haskell", "mathematics"]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/marketing
