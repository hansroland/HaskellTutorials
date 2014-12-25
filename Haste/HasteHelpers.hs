-- --------------------------------------------------------------------------
-- HasteHelpers.hs - Some very basic helper functions for Haste
-- --------------------------------------------------------------------------
--
-- Some functions used in several little tutorials factored out
--      to avoid duplicate code
--
--
-- See: http://www.cse.chalmers.se/edu/year/2014/course/TDA555/haste.html
--
-- -------------------------------------------------------------------------

module HasteHelpers where

import Haste
import Haste.App

-- | Add the element as a div child
wrapDiv :: MonadIO m => Elem -> m Elem
wrapDiv e = do
    d <- newElem "div"
    addChild e d
    return d

-- | Add several elements
addChildren :: MonadIO m => Elem -> [Elem] -> m()
addChildren parent children = sequence_ [addChild c parent | c <- children]

-- | Add several elements from the second argument besides each other
--   to the first argument
row :: MonadIO m => Elem -> [Elem] -> m()
row = addChildren

-- | Add several from the second argument elements below each other to the
--    first argument
column :: MonadIO m => Elem -> [Elem] -> m()
column parent children = do
    cs <- sequence [wrapDiv c | c <- children]
    addChildren parent cs
