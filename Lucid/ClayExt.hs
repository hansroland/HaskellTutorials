-- ---------------------------------------------------------------------------
-- LucidExt.hs - Some little extensions to blaze
-- ---------------------------------------------------------------------------

module ClayExt 
   (module ClayExt
   ,module Clay)
   where

import System.IO
-- import Lucid

import Clay
import Data.Text.Lazy

{-
-- | Write a Lucid html value to a file
writeHtmlFile :: Show a => FilePath -> a -> IO ()
writeHtmlFile fn lucid = writeFile fn (show lucid)
-}

-- | Write a Clay value to a file
renderToCssFile :: FilePath -> Css -> IO ()
renderToCssFile fn css = writeFile fn $ unpack $ render css
