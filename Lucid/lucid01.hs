-- -------------------------------------------------------------------------
-- lucid01.hs  - A first look at lucid-html
-- -------------------------------------------------------------------------
--
-- See: http://jaspervdj.be/blaze/tutorial.html
--      http://chrisdone.com/posts/lucid
--      https://github.com/chrisdone/lucid
--
-- -------------------------------------------------------------------------
-- {-# LANGUAGE OverloadedStrings #-}

import LucidExt
import Control.Monad (mapM_)

numbers :: Monad m => Int -> HtmlT m ()
numbers n = doctypehtml_ $ do
    head_ $ do
       title_ $ toHtml "Natural numbers"
    body_ $ do
       p_ $ toHtml "A list of natural numbers"
       ul_ $ mapM_ (li_ . toHtml . show) [1..n]
