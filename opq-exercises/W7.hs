module W7 where

-- See: https://github.com/opqdonut/haskell-exercises

import Data.List
import Control.Monad.State

-- Week 7: recap

-- Ex 1: implement the function pyramid that draws a pyramid like this:
--
--      *
--     ***
--    *****
--   *******
--  *********
-- ***********
--
-- The function is given the height of the pyramid as its argument.
--
-- Examples:
--   pyramidi 1 ==> "*\n"
--   pyramidi 2 ==> " *\n***\n"
--   pyramidi 3 ==> "  *\n ***\n*****\n"
--
-- PS. you can test the the function like this in ghci: putStr (pyramidi 5)

pyramid :: Int -> String
pyramid n = pyr 1 n

pyr l n 
  | l == n = stars l
  | otherwise = (replicate (n-l) ' ') ++ (stars l) ++ pyr (l + 1) n

stars l = (replicate (l*2 - 1) '*') ++ "\n"

-- Ex 2: collect every second element from the given list.
--
-- DO NOT use list functions, only pattern matching and recursion.
--
-- Examples:
--  everySecond [1,2,3,4,5]
--    ==> [1,3,5]
--  everySecond [0,7,8,1,4,2]
--    ==> [0,8,4]
--  everySecond []
--    ==> []

everySecond :: [a] -> [a]
everySecond [] = []
everySecond (x : xs) = x : ev2 xs
  where
    ev2 [] = []
    ev2 (a : as) = everySecond as


-- Ex 3: given a list, return a pair of functions (get,wrap) such that
--   * get i -- returns element i of the list
--   * query x -- returns True if x is contained in the list
--
-- Example:
--  let (get,query) = wrap [5,6,7] in (get 0, query 6, get 2, query 2)
--    ==> (5,True,7,False)

wrap :: Eq a => [a] -> (Int -> a, a -> Bool)
wrap xs = ((xs !!) , \x -> any ( x ==) xs) 

-- Tehtävä 4: Toteuta funktio nousevat, joka pilkkoo lukulistan
-- (aidosti) nouseviin pätkiin.
--
-- Saat käyttää kaikkia standardikirjaston listafunktioita.
--
-- Esimerkkejä:

-- Ex 4: split the given list into (monotonically) increasing pieces.
--
-- Feel free to use any list functions.
--
-- Examples:
--  increasings [1,2,3] ==> [[1,2,3]]
--  increasings [1,1] ==> [[1],[1]]
--  increasings [4,7,9,3,6,1,2,2,5,8,0]
--    ==> [[4,7,9],[3,6],[1,2],[2,5,8],[0]]

increasings :: [Int] -> [[Int]]
increasings [] = []
increasings xs = fs : increasings gs
    where 
      fs = incr [] xs
      gs = drop (length fs) xs

-- | incr the longest increasing sublist from the start
incr :: [Int] ->  [Int] -> [Int]
incr [] [] = []
incr [] [x] = [x]
incr [] (x : xs) = incr [x] xs
incr as [] = as
incr as (x : xs) 
   | (last as < x) = incr (as ++ [x]) xs
   | otherwise = as  

-- Ex 5: define a datatype Student that holds three pieces of
-- information about a student: a name (a String), a student number (a
-- String) and points (Int).
--
-- Also define the functions:
--   * newStudent name number -- returns a student with the given name
--                               and number and 0 points
--   * getName s -- returns the name of s
--   * getNumber s -- returns the student number of s
--   * getPoints s -- returns the points of s
--   * addPoints i s -- adds i points to s. If i is negative, does nothing.
--
-- Examples:
--  getName $ newStudent "frank" "0123"
--    ==> "frank"
--  getNumber $ newStudent "frank" "0123"
--    ==> "0123"
--  getPoints $ newStudent "frank" "0123"
--    ==> 0
--  getPoints $ addPoints 100 $ addPoints 100 $ newStudent "frank" "0123"
--    ==> 200
--  getPoints $ addPoints (-1000) $ newStudent "x" "0"
--    ==> 0

data Student = Student String String Int

newStudent :: String -> String -> Student
newStudent nam num = Student nam num 0

getName :: Student -> String
getName  (Student nam _ _) = nam 

getNumber :: Student -> String
getNumber (Student _ num _) = num

getPoints :: Student -> Int
getPoints (Student _ _ pts) = pts

addPoints :: Int -> Student -> Student
addPoints x (Student nam num pts) 
  | x > 0     = (Student nam num (pts + x))
  | otherwise = (Student nam num pts)

-- Ex 6: define a type Tree23 that represents a tree where each
-- (internal) node has 2 or 3 children.
--
-- The nodes don't need to contain any additional fields
--
-- Define the functions treeHeight and treeSize that compute the
-- height and size of a Tree23.
--
-- To facilitate testing, also define the functions node2 and node3
-- that create 2- and 3- child nodes, and the value leaf that
-- represents a leaf.
--
-- PS. Leave the "deriving Show" line intact because the tests want to
-- print out trees

data Tree23 = Leaf
    | Node2 Tree23 Tree23
    | Node3 Tree23 Tree23 Tree23
  deriving Show

leaf :: Tree23
leaf = Leaf
node2 :: Tree23 -> Tree23 -> Tree23
node2 t1 t2 = Node2 t1 t2
node3 :: Tree23 -> Tree23 -> Tree23 -> Tree23
node3 t1 t2 t3 = Node3 t1 t2 t3

treeHeight :: Tree23 -> Int
treeHeight Leaf = 0
treeHeight (Node2 t1 t2) = 1 +  max (treeHeight t1) (treeHeight t2)
treeHeight (Node3 t1 t2 t3) = 
    1 + max (max (treeHeight t1) (treeHeight t2)) (treeHeight t3)

treeSize :: Tree23 -> Int
treeSize Leaf = 0
treeSize (Node2 t1 t2) = 1 + (treeSize t1) + (treeSize t2)
treeSize (Node3 t1 t2 t3) = 1 +  (treeSize t1) + (treeSize t2) + (treeSize t3)

-- Ex 7: define a type MyString that represents a string and Eq and
-- Ord instances for it.
--
-- Also define the functions fromString and toString that convert
-- from/to String.
--
-- The Ord MyString instance should order the strings in
-- _lexicographic_ order. This means shorter strings come before
-- longer strings, and strings of the same length come in alphabetic
-- order.
--
-- You're free to choose the implmenetation of MyString as you wish.
--
-- Examples:
--
-- fromString "xyz" == fromString "xyz"          ==> True
-- fromString "xyz" == fromString "xyw"          ==> False
--
-- compare (fromString "abc") (fromString "ab")  ==> GT
-- compare (fromString "abc") (fromString "abd") ==> LT

data MyString = MyString String

fromString :: String -> MyString
fromString s = MyString s
toString :: MyString -> String
toString (MyString s) = s

instance Eq MyString where
   (MyString s1) == (MyString s2) = s1 == s2

instance Ord MyString where
    compare  (MyString s1) (MyString s2) = 
       case compare (length s1) (length s2) of
         EQ -> compare s1 s2
         LT -> LT
         GT -> GT

-- Ex 8: below you'll find a type Expr that represents arithmetic
-- expressions. For instance (1+2)/3+4 would be represented as
--   Plus (Div (Plus (Constant 1) (Constant 2)) (Constant 3)) (Constant 4)
--
-- Implement the function safeEval :: Expr -> Maybe Int that computes
-- the value of the given arithmetic expression. safeEval should
-- return Nothing if a division by zero occurs somewhere along the
-- way.
--
-- Hint: the Maybe-monad
--
-- Examples:
--   safeEval (Plus (Constant 1) (Constant 1))
--     ==> Just 2
--   safeEval (Div (Constant 6) (Constant 2))
--     ==> Just 3
--   safeEval (Div (Constant 6) (Constant 0))
--     ==> Nothing
--   safeEval (Plus (Constant 1) (Div (Constant 8) (Plus (Constant 2) (Constant (-2)))))
--     ==> Nothing

data Expr = Constant Int | Plus Expr Expr | Div Expr Expr
  deriving Show

safeEval :: Expr -> Maybe Int
safeEval (Constant n) = Just n
safeEval (Plus e1 e2) = liftM2 (+) (safeEval e1) (safeEval e2)
safeEval (Div e1 e2) = do
   q <- safeEval e1
   d <- safeEval e2
   if d == 0
   then Nothing
   else return $ div q d


-- Ex 9: implement the function test that gets a list of monadic
-- predicates (of type Monad m => a -> m Bool) and a value (of type
-- a). The predicates should be run on the value until a predicate
-- that returns False is found.
--
-- test should return False if one of the predicates returns False, or
-- True if all of the predicates passed.
--
-- Examples:
--
-- Simple Maybe-tests:
--  test [test1 2, test1 3, test1 5] 7
--   ==> Just True
--  test [test1 2, test1 3, test1 5] 4
--   ==> Just False
--  test [test1 2, test1 3, failTest] 4
--   ==> Nothing
--  test [test1 2, test1 3, failTest] 1
--   ==> Just False
--
-- Keeping track of tests run using State:
--  runState (test [test2 4, test2 8, test2 10] 11) []
--   ==> (True,[10,8,4])
--  runState (test [test2 4, test2 8, test2 10] 5) []
--   ==> (False,[8,4])
--  runState (test [test2 4, test2 8, test2 10] 0) []
--   ==> (False,[4])

test1 :: Int -> Int -> Maybe Bool
test1 k x = Just (x>k)

failTest :: Int -> Maybe Bool
failTest x = Nothing

test2 :: Int -> Int -> State [Int] Bool
test2 k x = do modify (k:)
               return (x>k)

test :: Monad m => [a -> m Bool] -> a -> m Bool
test [] _ = return True
test (p : ps) x = do
    b <- p x
    if b then (test ps x) else return False 







-- Ex 10: using the State monad, create a state with the elements that
-- occur in the given list an _odd_ number of times.
--
-- The order of the list doesn't matter.
--
-- Examples:
--  runState (odds [1,2,3,1,2,1]) []
--    ==> ((),[1,3])
--  runState (odds [1,2,3,1,2,3,1,2,3]) []
--    ==> ((),[3,2,1])

odds :: Eq a => [a] -> State [a] ()
odds xs = do
   mapM stateToggle xs
   return ()


stateToggle :: Eq a =>  a -> State [a] ()
stateToggle x = do
   st  <- get
   put $ toggle x st
   return ()

-- | If the element is in the list we remove it, otherwise we add it
toggle :: Eq a => a -> [a] -> [a]
toggle x xs = case elem x xs of
   True  -> delete x xs
   False -> x : xs



     
