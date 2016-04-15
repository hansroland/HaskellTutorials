-- -------------------------------------------------------------------------
-- A first Scotty program to set up the project
-- -------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Directories
import Control.Monad.Trans (liftIO)
import Control.Monad

import Text.Blaze.Html.Renderer.Text

import qualified Data.Text.Lazy as T

-- import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 as H hiding (main)

main = scotty 3000 $
  get "/" $ do
    txt <- liftIO $ liftM  blazeDirs $ subdirs ""
    Web.Scotty.html $ renderHtml txt

-- Note: liftM lifts the function blazeDirs into the IO monad
-- of the subdirs result

htmlSubdirs :: [String] -> T.Text
htmlSubdirs = T.pack . concat


blazeDirs :: [String] -> Html
blazeDirs as = H.docTypeHtml $ do
    H.head $
      H.title "Hello!"
    H.body $ do
      H.h1 "Title"
      H.p ("Hello " >> toHtml (Prelude.head as) >> "!")
