-- -------------------------------------------------------------------------
-- A first Scotty program to show some directories
-- -------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Directories
import Data.Text
import Lucid

main = do
  dirs <- subdirs ""
  scotty 3000 $
    get "/" . html . renderText $ do
      doctype_
      myPage1 $ fmap pack dirs

myPage1 :: [Text] -> Html ()
myPage1 ar = do
      head_ $
        title_ "Title"
      body_ $ do
        h1_ "Directories"
        ul_ (mapM_ (li_ . toHtml ) ar)
