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
    input  <- newElem "input"
    button <- newElem "button"
    setProp input "type" "text"
    setProp input "size" "30"
    setProp input "value" "Type your answer here ..."
    setProp button "innerHTML" "Submit answer"
    column documentBody [input, button]


