-- ------------------------------------------------------------------------
-- Interact.hs - First Haste example with some interaction
-- ------------------------------------------------------------------------
-- 
-- See: http://www.cse.chalmers.se/edu/year/2014/course/TDA555/haste.html
--
-- Compile with: hastec --output-html Interact.hs
--
-- -------------------------------------------------------------------------
import Haste
import HasteHelpers

main = do
    input  <- mkInput 30 "Type your answer here ..."
    button <- mkButton "Submit answer"
    column documentBody [input, button]


