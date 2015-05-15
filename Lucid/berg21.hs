-- ------------------------------------------------------------------------
-- berg21.html
-- ------------------------------------------------------------------------
--
-- See: Olaf Bergmann, Carsten Borman: Ajax Frische Ansätze für das Web Design
--
-- Beispiel 2.1 (p.37):
-- Try to reproduce with Lucid
--
-- use: renderToFile "berg21.html" berg21
--
-- ------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

import Lucid

berg21 :: Monad m => HtmlT m ()
berg21 = doctypehtml_ $ do
    head_ $ do
      title_ "Example 21"
      link_ [rel_ "stylesheet", href_ "berg21.css"]
    body_ $ do
      ul_ [id_ "nav"] $ do
         lihref "Home"
         lihref "Über uns" 
         ul_ $ do
            lihref "Unternehmen"       
            lihref "Management"       
            lihref "Geschichte"       
         lihref "Dienstleistungen"
         ul_ $ do
            lihref "Autowäsche"
            lihref "Bremsenprüfung"
            lihref "Ölwechsel"
            lihref "Lichttest"
         lihref "Kontakt"
         ul_ $ do
            lihref "Postanschrift"
            lihref "E-Mail Formular"
            lihref "Anfahrtsskizze"


lihref ::  Monad m => String ->  HtmlT m ()
lihref txt = do
     li_ $ do
        a_ [href_ "#"] $ toHtml txt

