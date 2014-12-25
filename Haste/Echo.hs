-- ------------------------------------------------------------------------
-- Echo.hs - Copy text during typing from input field to an output field
-- ------------------------------------------------------------------------
-- 
-- See: http://www.cse.chalmers.se/edu/year/2014/course/TDA555/haste.html
--
-- Compile with: hastec --output-html Echo.hs
--
-- -------------------------------------------------------------------------
import Haste
import HasteHelpers

main = do
    input  <- mkInput 30 "Type your answer here ..."
    output <- newElem "span"
    column documentBody [input, output]

    onEvent input OnKeyUp $ \_  -> do
        text <- getProp input "value"
        setProp output "innerHTML" text

