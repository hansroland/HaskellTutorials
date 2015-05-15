-- ---------------------------------------------------------------------------
-- BlazeExt.hs - Some little extensions to blaze
-- ---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module LucidExt 
   (module LucidExt
   ,module Lucid)
   where

import System.IO
import Lucid


-- import Clay
-- import Data.Text.Lazy

-- writeHtmlFile :: FilePath -> HtmlT IO () -> IO ()
writeHtmlFile fn html = do
    -- putStrLn (renderHtml html)
    writeFile fn (show html)


-- writeClayFile :: FilePath -> Css -> IO ()
-- writeClayFile fn css = do
--    writeFile fn (unpack (render css))
     

