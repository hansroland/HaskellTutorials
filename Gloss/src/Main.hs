-- ---------------------------------------------------------------------------
-- Some Simple examples in Gloss
-- ---------------------------------------------------------------------------
--
-- See: http://hackage.haskell.org/package/gloss-1.9.2.1
--

import Graphics.Gloss

main :: IO()
main = prompt


prompt :: IO()
prompt = do
    putStrLn "Choose"
    putStrLn "1 -> gloss-easy"
    putStrLn "2 -> gloss-hello"
    answer <- getLine
    checkAnswer answer

checkAnswer :: String -> IO()
checkAnswer "1" = glossEasy
checkAnswer "2" = glossHello
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
	picture			 -- picture to display


-- Note: the text starts in the middle of the window
picture :: Picture
picture
	= Translate (-170) (-20) -- shift the text to the middle of the window
	$ Scale 0.5 0.5		     -- display it half the original size
	$ Text "Hello World"	 -- text to display




