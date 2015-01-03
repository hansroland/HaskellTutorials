-- ---------------------------------------------------------------------
-- Snowmen.hs - First graphical example
-- ---------------------------------------------------------------------
-- 
-- See: http://www.cse.chalmers.se/edu/year/2014/course/TDA555/haste.html
--
-- Compile with: hastec --output-html Snowmen.hs
--
-- -------------------------------------------------------------------------

import Haste
import Haste.Graphics.Canvas

import HasteHelpers


-- Create a single snowman picture x = how far right we draw the snow man
snowMan :: Double -> Shape ()
snowMan x = do
    circle (x, 100) 20
    circle (x, 65)  15
    circle (x, 40)  10

twoSnowMenInABox :: Picture ()
twoSnowMenInABox = do
    fill   $ snowMan 100
    stroke $ snowMan 200
    stroke $ rect (50,10) (250,150)


main :: IO()
main = do
    canvas <- mkCanvas 300 300
    addChild canvas documentBody
    Just can <- getCanvas canvas
    render can twoSnowMenInABox
