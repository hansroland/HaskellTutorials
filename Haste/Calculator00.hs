-- ------------------------------------------------------------------------
-- Calculator00.hs - A very simple Haste calculator (it just adds 2 numbers)
-- ------------------------------------------------------------------------
--
-- See: http://www.cse.chalmers.se/edu/year/2014/course/TDA555/haste.html
--
-- Compile with: hastec --output-html Calculator00.hs
--
-- -------------------------------------------------------------------------

import Haste
import Haste.App (MonadIO)
import HasteHelpers


main = do
    inp1 <- mkInput 10 ""
    lab1 <- mkLabel " + "
    inp2 <- mkInput 10 ""
    lab2 <- mkLabel " = "
    output <- mkLabel ""
    row documentBody [inp1, lab1, inp2, lab2, output]

    -- Event handlers
    onEvent inp1 OnKeyUp $ \_  -> calculate inp1 inp2 output
    onEvent inp2 OnKeyUp $ \_  -> calculate inp1 inp2 output

-- | Calculate a new sum after an evemt
calculate :: MonadIO m => Elem -> Elem -> Elem -> m()
calculate inp1 inp2 out = do
    val1 <- getVal inp1
    val2 <- getVal inp2
    showIntVal out (val1 + val2)
   
-- | Attention : read of an empty string does not return a string
myRead :: String -> Int
myRead "" = 0
myRead n  = read n

getVal :: MonadIO m => Elem -> m Int
getVal elem = do
   text <- getProp elem "value"
   return $ (myRead text)

showIntVal ::  MonadIO m => Elem -> Int -> m ()
showIntVal elem val = do
   setProp elem "innerHTML" (show val)

