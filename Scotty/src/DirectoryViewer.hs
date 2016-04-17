-- -------------------------------------------------------------------------
-- A first Scotty program to show some directories
-- -------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Directories
import Data.Text
import Control.Monad.Trans (liftIO)
import Lucid

-- |
main =
  scotty 3000 routes

-- | The routing table
routes :: ScottyM ()
routes = do
  get "/" showParent
  get "/dir/:dirName" $ do
    dir <- param "dirName"
    showSubdir dir

-- | Handler when no parameter is given -> show first page
showParent :: ActionM ()
showParent = do
     dirs <- liftIO $ subdirs ""
     html $ renderText $ page1 $ fmap pack dirs

-- | Handler when a dir parameter is given
--   http://localhost:3000/dir/test
showSubdir :: Text -> ActionM ()
showSubdir dir =
    html $ renderText $ page2 dir

-- | Show the first page
page1 :: [Text] -> Html ()
page1 ar = do
      head_ $
        title_ "Title"
      body_ $ do
        h1_ "Directories"
        ul_ (mapM_ (li_ . toHtml ) ar)

-- | Show the second page
page2 :: Text -> Html ()
page2 dir = do
  head_ $
    title_ "Title"
  body_ $ do
    h1_ "Page 2 "
    h2_ $ toHtml dir
