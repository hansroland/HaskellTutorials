module W4 where

-- See: https://github.com/opqdonut/haskell-exercises

-- To avoid the resource exception (Too many open files) use:
-- ulimit -n 4096

import Control.Monad
import Data.List
import Data.IORef
import System.IO

-- Week 4:
--   * The IO type
--   * do-notation
--
-- Useful functions / operations:
--   * putStrLn
--   * getLine
--   * readLn
--   * replicateM
--   * readFile
--   * lines
--
-- If these exercises feel weird or hard, feel free to skip this week for now

-- Ex 1: define an IO operation hello that prints two lines. The
-- first line should be HELLO and the second one WORLD

hello :: IO ()
hello = do
   putStrLn "HELLO"
   putStrLn "WORLD"

-- Ex 2: define the IO operation greet that takes a name as an
-- argument and prints a line "HELLO name".

greet :: String -> IO ()
greet name = do
   putStrLn ("HELLO " ++ name)


-- Ex 3: define the IO operation greet2 that reads a name from the
-- keyboard and then greets that name like the in the previous
-- exercise.
--
-- Try to use the greet operation in your solution.

greet2 :: IO ()
greet2 = do
    name <- getLine 
    greet name

-- Ex 4: define the IO operation readWords n which reads n lines from
-- the user and returns them in alphabetical order.

readWords :: Int -> IO [String]
readWords n = do
   rs <- replicateM n  getLine
   return $ sort rs

-- Ex 5: define the IO operation readUntil f, which reads lines from
-- the user and returns them as a list. Reading is stopped when f
-- returns True for a line. (The value for which f returns True is not
-- returned.)

readUntil :: (String -> Bool) -> IO [String]
readUntil f = accum f []
  where
    accum :: (String -> Bool) -> [String] -> IO [String]
    accum f lst = do
        l <- getLine
        if f l
        then return $ reverse lst
        else accum f (l:lst)

-- Ex 6: given n, print the n first fibonacci numbers, one per line

printFibs :: Int -> IO ()
printFibs n = do
  mapM_ (putStrLn . show) (take n fibs) 
  where
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- Ex 7: isums n should read n numbers from the user and return their
-- sum. Additionally, after each read number, the sum up to that
-- number should be printed.

isums :: Int -> IO Int
isums n = accum n 0
  where
    accum :: Int -> Int -> IO Int
    accum n x = do
       y <- getLine
       let r = x + (read y)
       putStrLn $ show (r)
       if n == 1 
          then return r
          else accum (n-1) r
 
-- Ex 8: when is a useful function, but its first argument has type
-- Bool. Write a function that behaves similarly but the first
-- argument has type IO Bool.

whenM :: IO Bool -> IO () -> IO ()
whenM cond op = do
   b <- cond
   when b op
     

-- Ex 9: implement the while loop. while condition operation should
-- run operation as long as condition returns True.
--
-- Examples:
-- while (return False) (putStrLn "IMPOSSIBLE")  -- prints nothing
--
-- let ask :: IO Bool
--     ask = do putStrLn "Y/N?"
--              line <- getLine
--              return $ line == "Y"
-- in while ask (putStrLn "YAY!")
--
-- This prints YAY! as long as the user keeps answering Y

while :: IO Bool -> IO () -> IO ()
while cond op = do
       b <- cond
       if b
         then do
           op
           while cond op
         else return ()
           

-- Ex 10: given a string and an IO operation, print the string, run
-- the IO operation, print the string again, and finally return what
-- the operation returned.
--
-- Note! the operation should be run only once
--
-- Examples:
--   debug "CIAO" (return 3)
--     - prints two lines that contain CIAO
--     - returns the value 3
--   debug "BOOM" getLine
--     1. prints "BOOM"
--     2. reads a line from the user
--     3. prints "BOOM"
--     4. returns the line read from the user

debug :: String -> IO a -> IO a
debug s op = do
    putStrLn s
    a <- op
    putStrLn s
    return a

-- Ex 11: Reimplement mapM_ (specialized to the IO type) using
-- recursion and pattern matching.
--
-- In case you don't know what mapM_ does, it takes a parameterized IO
-- operation and a list of parameters, and runs the operation for each
-- value in the list.

mymapM_ :: (a -> IO b) -> [a] -> IO ()
mymapM_ _ []       = return ()
mymapM_ f (x : xs) = (f x) >> mymapM_ f xs
    

-- Ex 12: Reimplement the function forM using pattern matching and
-- recursion.

myforM :: [a] -> (a -> IO b) -> IO [b]
myforM [] f       = return []
myforM (x : xs) f = do 
      y <- f x
      ys <- myforM xs f
      return $ y : ys

-- Ex 13: sometimes one bumps into IO operations that return IO
-- operations. For instance the type IO (IO Int) means an IO operation
-- that returns an IO operation that returns an Int.
--
-- Implement the function doubleCall which takes an operation op and
--   1. runs op
--   2. runs the operation returned by op
--   3. returns the value returned by this operation
--
-- Examples:
--   - doubleCall (return (return 3)) is the same as return 3
--
--   - let op :: IO (IO [String])
--         op = do l <- readLn
--                 return $ replicateM l getLine
--     in doubleCall op
--
--     works just like
--
--     do l <- readLn
--        replicateM l getLine

tuplaKutsu :: IO (IO a) -> IO a
tuplaKutsu op2 = do
     op2
     op1 <- op2
     op <- op1
     return op

-- Ex 14: implement the analogue of function composition (the (.)
-- operator) for IO operations. That is, take an operation op1 of type
--     a -> IO b
-- an operation op2 of type
--     c -> IO a
-- and a value of type
--     c
-- and returns an operation op3 of type
--     IO b
--
-- op3 should of course
--   1. take the value of type c and pass it to op2
--   2. take the resulting value (of type a) and pass it to op1
--   3. return the result (of type b)

compose :: (a -> IO b) -> (c -> IO a) -> c -> IO b
compose op1 op2 c = (op2 c) >>= op1

-- Ex 15: take a look at the documentaiton for Data.IORef
-- <http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-IORef.html>
--
-- Implement the function mkCounter that returns the io operations
-- inc :: IO () and get :: IO Int. These operations should work like this:
--
--   get returns the number of times inc has been called
--
-- In other words, a simple stateful counter.
--
-- An example of how mkCounter works in GHCi:
--
--  *W4> (inc,get) <- mkCounter
--  *W4> inc
--  *W4> inc
--  *W4> get
--  2
--  *W4> inc
--  *W4> inc
--  *W4> get
--  4

mkCounter :: IO (IO (), IO Int)
mkCounter = do
   myref <- newIORef (0 :: Int)
   return $ (modifyIORef myref  (+1), readIORef myref)
    

-- Ex 16: fetch from the given file (Handle) the lines with the given
-- indices. Line indexing starts from 1. You can assume that the
-- numbers are given in ascending order.
--
-- Have a look at the docs for the System.IO module for help.

hFetchLines :: Handle -> [Int] -> IO [String]
hFetchLines h nums = do 
    ls <- hFetchLines' h nums 0
    hClose h
    return ls

hFetchLines' :: Handle -> [Int] -> Int -> IO [String]
hFetchLines' _ [] p = return []
hFetchLines' h (n : ns) p = do
    l <- hGetAfterReads h (n - p)
    ls <- hFetchLines' h ns n
    return (l : ls)
     
hGetAfterReads :: Handle -> Int ->  IO(String)
hGetAfterReads h n = do
   x <- hGetLine h
   if (n == 1)
      then return x
      else hGetAfterReads h  (n - 1)

-- Ex 17: CSV is a file format that stores a two-dimensional array of
-- values in a file. Each row of the file is a row of the array. Each
-- row of the file consists of values on that row separated with the ,
-- character.
--
-- Implement the function readCSV that reads a CSV file and returns it
-- as a list of lists.
--
-- NB! You don't need to handle the intricacies of real CSV, e.g.
-- quoting. You can assume each , character starts a new field.
--
-- NB! The lines might have different numbers of elements.


readCSV :: FilePath -> IO [[String]]
readCSV path = do
    dat <- readFile path
    let ls = lines dat
    return $ map csv ls

csv s = 
       let (s1, s2) = break (== ',') s
       in s1 : case s2 of
          []         -> []
          (_ : s2')  -> csv s2'


-- Ex 18: your task is to compare two files, a and b. The files should
-- have the same contents, but if lines at index i differ from each
-- other, you should print
--
-- < file a version of the line
-- > file b version of the line
--
-- Example:
--
-- File a contents:
-- a
-- aa
-- x
-- aa
-- bb
-- cc
--
-- File b contents:
-- a
-- aa
-- bb
-- aa
-- cc
-- dd
--
-- Output:
-- < x
-- > bb
-- < bb
-- > cc
-- < cc
-- > dd
--
-- NB! You can assume the files have the same number of rows.
--
-- Hint! It's probably wise to implement a pure function for finding
-- the differing lines. A suitable type could be
-- [String] -> [String] -> [String].

compareFiles :: FilePath -> FilePath -> IO ()
compareFiles f1 f2 = do
     h1 <- openFile f1 ReadMode
     h2 <- openFile f2 ReadMode
     txt1 <- hGetContents h1
     txt2 <- hGetContents h2
     mapM putStrLn $ compareTexts (lines txt1) (lines txt2)
     hClose h1
     hClose h2
     return ()
       where
         compareTexts :: [String] -> [String] -> [String]
         compareTexts ls1 ls2 = concat $ zipWith compareLines ls1 ls2
         compareLines :: String -> String -> [String]
         compareLines l1 l2 
           | l1 == l2 = []
           | otherwise = ("< " ++ l1) : ("> " ++ l2) : []

-- Ex 19: In this exercise we see how a program can be split into a
-- pure part that does all of the work, and a simple IO wrapper that
-- drives the pure logic.
--
-- Implement the function interact' that takes a pure function f of
-- type
--   (String, st) -> (Bool, String, st)
-- and a starting state of type st and returns an IO operation of type
-- IO st
--
-- interact' should read a line from the user, feed the line and the
-- current state to f. f then returns a boolean, a string to print and
-- a new state. The string is printed, and if the boolean is True, we
-- continue running with the new state. If the boolean is False, the
-- execution has ended and the state should be returned.
--
-- Example:
--
-- let f :: (String,Integer) -> (Bool,String,Integer)
--     f ("inc",n)   = (True,"",n+1)
--     f ("print",n) = (True,show n,n)
--     f ("quit",n)  = (False,"bye bye",n)
-- in interact' f 0
--

interact' :: ((String,st) -> (Bool,String,st)) -> st -> IO st
interact' f state0 = do
    line0 <- getLine
    let (b, line1, state1) = f (line0, state0)
    mapM_ putChar line1
    if b
    then interact' f state1
    else return state1
