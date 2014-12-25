-- ------------------------------------------------------------------------
-- SimpleText.hs: A second example of a Haste page
-- ------------------------------------------------------------------------
-- See: http://www.cse.chalmers.se/edu/year/2014/course/TDA555/haste.html
--
-- Compile with: hastec --output-html SimpleText.hs
--
-- ------------------------------------------------------------------------

import Haste

import Haste.App

-- | The main function creates a very simple page layout
main = do
    text1 <- newTextElem "Top Left "
    text2 <- newTextElem "Top Right "
    text3 <- newTextElem "Bottom Left "
    text4 <- newTextElem "Bottom Right "
    -- define the rows
    topRow <- newElem "div"
    bottomRow <- newElem "div"
    row topRow [text1, text2]
    row bottomRow [text3, text4]
    column documentBody [topRow, bottomRow]

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
