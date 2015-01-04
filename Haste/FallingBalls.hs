-- ---------------------------------------------------------------------
-- FallingBalls.hs - First animation example
-- ---------------------------------------------------------------------
-- 
-- See: http://www.cse.chalmers.se/edu/year/2014/course/TDA555/haste.html
--
-- Compile with: hastec --output-html FallingBalls.hs
--
-- -------------------------------------------------------------------------

import Haste
import Haste.Graphics.Canvas

import HasteHelpers

-- create a picture from a shape
ball :: Double -> Picture ()
ball y = fill $ circle (100,y) 10

fall :: Canvas -> Elem -> Int -> IO ()
fall can boom v = do
    render can $ ball y
    if y < 600
      then setTimeout 20 $ fall can boom (v+1)
      else setProp boom "innerHTML" "!!! *** BOOM *** !!!"
  where
    v' = fromIntegral v
    y  = 0.03 * v'^2

main :: IO ()
main = do
    canvas <- mkCanvas 300 600
    boom   <- newElem "span"
    column documentBody [canvas,boom]
    setStyle boom "fontSize" "150%"
    Just can <- getCanvas canvas
    fall can boom 0



