-- ----------------------------------------------------------------------------
-- table01.hs - First Lucid/Clay examples with a HTML table
-- ----------------------------------------------------------------------------
--
-- use: renderToFile "table01.html" page
--      

{-# LANGUAGE OverloadedStrings #-}

import Lucid

page :: Monad m => HtmlT m ()
page = doctypehtml_ $ do
    head_ $ do
      title_ "Simple HTML Tables"
      link_ [rel_ "stylesheet", href_ "tables.css"]
    body_ $ do
      p_ ""
      table01
      p_ ""
      table02


-- | A first table without a table heading
table01 :: Monad m => HtmlT m ()
table01 = do
   table_ [class_ "tblSimple"] $ do
     tr_ $ do
       td_ "Row 1 Column 1"
       td_ "Row 1 Column 2"
     tr_ $ do
       td_ "Row 2 Column 1"
       td_ "Row 2 Column 2"

-- | A second table with a table heading
table02 :: Monad m => HtmlT m ()
table02 = do
   table_ [class_ "tblText"] $ do
     tr_ $ do
       th_ [width_ "80%"] "Name"
       th_ [width_ "20%"] "Salary"
     tr_ $ do
       td_ "Ramesh"
       td_ "80000"
     tr_ $ do
       td_ "Hussein"
       td_ "50000"
     tr_ $ do
       td_ "Meyer"
       td_ "55000"
