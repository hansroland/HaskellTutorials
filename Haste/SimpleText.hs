-- ------------------------------------------------------------------------
-- SimpleText.hs: A second example of a Haste page
-- ------------------------------------------------------------------------
-- See: http://www.cse.chalmers.se/edu/year/2014/course/TDA555/haste.html
--
-- Compile with: hastec --output-html SimpleText.hs
--
-- ------------------------------------------------------------------------

import Haste

import HasteHelpers

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

