-- -------------------------------------------------------------------------
-- A webserver to serve static pages
-- -------------------------------------------------------------------------
--
-- This is the most simple web server.
-- It serves static files in the current directory and its subdirectories
--
--
-- use:
--  >  staticPolicy (noDots >-> addBase "myDir")
-- to serve files form the myDir directory. This directory is relative
-- to the current directory. (current directory ist the directory
-- the web server was started in.
--
-- Note:
-- sudo is needed if the port is below 1024
-- For my installation this is
-- >>>  sudo ~/bin/WebServer

{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Lucid
import Network.Wai.Middleware.Static

-- | main function
main = scotty port $ do
  middleware $ staticPolicy noDots
  get "/" . html . renderText $ do
    doctype_
    head_ $
      title_  "Scotty"
    body_ $
      p_  "Hello World"

-- | little helper function to return the port
port :: Int
port = 8080
