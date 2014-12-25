-- ------------------------------------------------------------------------
-- Dom01.hs - First Haste example with some interaction
-- ------------------------------------------------------------------------
-- 
-- See: http://www.cse.chalmers.se/edu/year/2014/course/TDA555/haste.html
--
-- Compile with: hastec --output-html Dom01.hs
--
-- Open resulting file in the browser
--
-- -------------------------------------------------------------------------

import Haste
import HasteHelpers

main = do
    text1 <- newTextElem "This is some text."
    text2 <- newTextElem "This is another text"
    row documentBody [text1, text2]
