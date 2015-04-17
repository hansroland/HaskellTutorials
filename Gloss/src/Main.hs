-- ---------------------------------------------------------------------------
-- Some Simple examples in Gloss
-- ---------------------------------------------------------------------------
--
-- See: http://hackage.haskell.org/package/gloss-1.9.2.1
--

{-# LANGUAGE BangPatterns #-}

import Graphics.Gloss


main :: IO()
main = prompt


prompt :: IO()
prompt = do
    putStrLn "Choose"
    putStrLn "1 -> gloss-easy"
    putStrLn "2 -> gloss-hello"
    putStrLn "3 -> gloss-flake"
    answer <- getLine
    checkAnswer answer

checkAnswer :: String -> IO()
checkAnswer "1" = glossEasy
checkAnswer "2" = glossHello
checkAnswer "3" = glossFlake
checkAnswer _   = prompt
    
-- --------------------------------------------------------------------------
-- glossEasy - The first most simple window
-- --------------------------------------------------------------------------
glossEasy :: IO()
glossEasy = display (InWindow "My Window" (400, 400) (100, 200)) red (Circle 80)

-- FullScreen does not work !!
-- glossEasy = display (FullScreen (400, 400)) red (Circle 100)


-- --------------------------------------------------------------------------
-- glossHello - Display Hello in a window
-- --------------------------------------------------------------------------
glossHello :: IO()
glossHello = display
        (InWindow
	       "Hello World" 	 -- window title
		(400, 150) 	 -- window size
		(10, 10)) 	 -- window position
	white			 -- background color
	textPicture		 -- picture to display


-- Note: the text starts in the middle of the window
textPicture :: Picture
textPicture
	= Translate (-170) (-20) -- shift the text to the middle of the window
	$ Scale 0.5 0.5		     -- display it half the original size
	$ Text "Hello World"	 -- text to display

-- ----------------------------------------------------------------------------
-- | Snowflake Fractal.
-- ---------------------------------------------------------------------------
glossFlake :: IO()
glossFlake = display (InWindow "Snowflake" (500, 500) (20,  20))
	       black (flakePicture 4)

-- Fix a starting edge length of 360
edge :: Float
edge = 360

-- Move the fractal into the center of the window and colour it nicely
flakePicture :: Int -> Picture
flakePicture degree
	= Color aquamarine
	$ Translate (-edge/2) (-edge * sqrt 3/6)
	$ snowflake degree

-- The fractal function
side :: Int -> Picture
side 0 = Line [(0, 0), (edge, 0)]
side n
 = let	newSide = Scale (1/3) (1/3)
		$ side (n-1)
   in	Pictures
		[ newSide
		, Translate (edge/3) 0 			  $ Rotate 60    newSide
		, Translate (edge/2) (-(edge * sqrt 3)/6) $ Rotate (-60) newSide
		, Translate (2 * edge/3) 0		  $ newSide ]

-- Put 3 together to form the snowflake
snowflake :: Int -> Picture
snowflake n
 = let	oneSide	= side n
   in 	Pictures
		[ oneSide
		, Translate edge 0 			$ Rotate (-120) $ oneSide
		, Translate (edge/2) (edge * sqrt 3/2)	$ Rotate 120	$ oneSide]









