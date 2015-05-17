-- ---------------------------------------------------------------------------
-- PhytagorianTripples.hs   How to find the 
-- ---------------------------------------------------------------------------
--
-- 
import Control.Monad

-- The list monad is good for taking independent samples of values.
-- 
-- eg Build all tripples : Cartesian Product !!
allTripples :: [a] -> [[a]]
allTripples lst = do
    x <- lst
    y <- lst
    z <- lst
    return [x,y,z]

-- >>> allTripples "abc" 
-- ["aaa","aab","aac","aba","abb","abc","aca","acb","acc",
--  "baa","bab","bac","bba","bbb","bbc","bca","bcb","bcc",
--  "caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"]

-- Filter out the symetric solutions
allAscTripples :: Ord a =>  [a] -> [[a]]
allAscTripples lst = do
    x <- lst
    y <- filter ((>=) x) lst
    z <- filter ((>=) y) lst
    return [x,y,z]

-- >>> allAscTripples "abc"
-- ["aaa","aab","aac","abb","abc","acc","bbb","bbc","bcc","ccc"]

-- This finally allows us to filter out the Phytagorian Tripples
-- with a guard from the MonadPlus instance
-- Note: with numbers, there is an other solution to filter out the
--       symetric solution
phytagorianTripples :: Int -> [[Int]]
phytagorianTripples n = do
    a <- [1..n]
    b <- [a..n]
    c <- [b..n]
    guard $ a*a + b*b == c*c
    return $ [a,b,c]

-- 

