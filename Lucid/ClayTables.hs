-- -------------------------------------------------------------
-- tables.hs - A first example of a css for textTables
-- -------------------------------------------------------------
--
-- use: renderToCssFile "tables.css" tables    
--      putCss tables                       -- just to test  
--

{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import ClayExt
import Data.Text.Lazy
import Prelude hiding ( (**) )

tables = do
  tblSimple
  tblText


-- | The simple table is just here to test the linking of the
--   html with the css file 
tblSimple :: Css
tblSimple = (element ".tblSimple") ? do
   background $ rgb 125 125 125 
   color      $ rgb 0 0 0 
   border     dashed (px 2) (rgb 255 255 0) 

-- | Create the Css for the simple text table
tblText :: Css
tblText = do
  element ".tblText" ? do 
     borderCollapse collapse
  (element ".tblText")  ** th ? do
        background $ rgb 180 180 180
        tblBorder
  (element ".tblText")  ** td ?  do
        background $ rgb 240 240 240
        tblBorder

-- | put a fine blank line between all table cells
tblBorder :: Css
tblBorder = do
        borderStyle solid
        borderWidth $ px 1
        borderColor black
        padding (px 10) (px 10) (px 10) (px 10)
  

