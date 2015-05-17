-- ---------------------------------------------------------------------------
-- SendMoreMoney.hs - Solve the puzzle in Haskell
-- ---------------------------------------------------------------------------
--
-- Solve the puzzle:
--
--     send
--  +  more
--    ----- 
--    money
--
--
-- See: 
-- http://blog.jle.im/entry/unique-sample-drawing-searches-with-list-and-statet
--
-- A way to solve combinatorical problems
--
-- ---------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Trans.State
import Data.List

-- 
-- The list monad is good for taking independent samples of values,
--    see PhytagorianTripples.hs in this directory
-- 
-- However there are problems where one can take out a value from the list
--    only once.
--    One poor solution is to check it with an Not-Equal filter
--    

poor :: Eq a => [a] -> [[a]]
poor lst = do
    x <- lst
    y <- filter ((/=) x) lst
    z <- filter ((/=) y) lst
    return [x,y,z]

-- A better solution uses a function that takes out the value and continues
--   only with the remainding list and use this in 
-- Note this function does not use an Eq constraint !!


-- select :: [a] -> [(a, [a])]
-- Rewrite the resulting list in List Monad notation !!
select :: [a] -> [] (a, [a])
select []       = []
select (x : xs) = (x,xs) : [(y, x:ys) | (y,ys) <- select xs]

-- Compare the type of select with
-- StateT select :: StateT [a] [] a

-- >>> select "abcd"
--     [('a',"bcd"),('b',"acd"),('c',"abd"),('d',"abc")]

-- | convert a lsit of digits to a number
asNumber :: [Int] -> Int
asNumber = foldl' (\t o -> t*10 + o) 0

-- | The main program for the send-more-money puzzle
main :: IO()
main = print . flip evalStateT [0..9] $ do
   s <- StateT select
   e <- StateT select
   n <- StateT select
   d <- StateT select
   m <- StateT select
   o <- StateT select
   r <- StateT select
   y <- StateT select
   guard $ s /= 0 && m /= 0
   let send = asNumber [s,e,n,d]
       more = asNumber [m,o,r,e]
       money = asNumber [m,o,n,e,y]
   guard $ send + more == money
   return (send, more, money)

