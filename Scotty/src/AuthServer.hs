-- ---------------------------------------------------------------------------
-- Simple Authentication with Scotty (Using the underlaying WAI Middleware)
-- ---------------------------------------------------------------------------
--
-- See: https://ro-che.info/articles/2016-04-14-scotty-http-basic-auth
--
-- Missing: This should be chagned form http to https.
--          Now the password is sent plain text ...
-- ---------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Network.Wai.Middleware.HttpAuth
import Data.SecureMem -- for constant-time comparison
import Lucid -- for HTML generation

password :: SecureMem
password = secureMemFromByteString "password" -- https://xkcd.com/221/

main :: IO ()
main = scotty 8000 $ do
  middleware $ basicAuth (\u p -> return $ u == "user" && secureMemFromByteString p == password)
    "Scotty"

  get "/" . html . renderText $ do
    doctype_
    html_ $ do
      head_ $
        title_ "Scotty authenticated"
      body_ $ h1_ "Hello world!"
