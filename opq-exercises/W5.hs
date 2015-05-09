module W5 where

-- See: https://github.com/opqdonut/haskell-exercises

import System.Random
import Data.List

-- Week 5:
--  - operators
--  - using typeclasses
--  - implementing typeclasses
--  - forcing/laziness
--
-- Useful type classes to know:
--  - Eq
--  - Ord
--  - Show
--  - Num
--  - Functor

-- Ex 1: hey, did you know you can implement your own operators in
-- Haskell? Implement the operator %$ that combines two strings like
-- this:
--
-- "aa" %$ "foo" ==> "aafooaa"
--
-- and the operator *! that takes a value and a number and produces a
-- list that repeats the value that many times:
--
-- True *! 3 ==> [True,True,True]

(%$) :: String -> String -> String
x %$ y = x ++ y ++ x

(*!) :: Int -> a -> [a]
n *! val = replicate n val

-- Ex 2: implement the function allEqual which returns True if all
-- values in the list are equal.
--
-- Examples:
--
-- allEqual [] ==> True
-- allEqual [1,2,3] ==> False
-- allEqual [1,1,1] ==> True
--
-- PS. check out the error message you get with your implementation if
-- you remove the Eq a => constraint from the type!

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual (x : y : zs) = (x == y) && (allEqual (y : zs))

-- Ex 3: implement the function secondSmallest that returns the second
-- smallest value in the list, or Nothing if there is no such value.
--
-- Examples:
--
-- secondSmallest [1.0] ==>  Nothing
-- secondSmallest [1,1] ==>  Just 1
-- secondSmallest [5,3,7,2,3,1]  ==>  Just 2

secondSmallest :: Ord a => [a] -> Maybe a
secondSmallest [] = Nothing
secondSmallest [x] = Nothing
secondSmallest lst = Just y
   where
     x = minimum lst
     y = minimum $ delete x lst


-- Ex 4: find how two lists differ from each other. If they have
-- different lengths, return
--   Just "<length of list> /= <length of other list>"
-- if they have the same length, find the first index i for which the
-- elements differ, and return
--   Just "<value at index i> /= <other value at index i>"
-- if the lists are the same, return
--   Nothing
--
-- NB! Write the type signature for findDifference your self. Which
-- type classes do you need?
--
-- Examples:
--  findDifference [True,False] [True,True]
--    ==> Just "False /= True"
--  findDifference [0,0,0] [0,0,0,0]
--    ==> Just "3 /= 4"

findDifference :: (Eq a, Show a) => [a] -> [a] -> Maybe String
findDifference [] [] = Nothing
findDifference [] ys = diffLength [] ys
findDifference xs [] = diffLength xs []
findDifference (x : xs) (y : ys) = 
   if (length xs) /= (length ys)
   then diffLength (x:xs) (y:ys)
   else if x == y
        then findDifference xs ys
        else Just ((show x) ++ " /= " ++ (show y))

diffLength xs ys = Just ((show (length xs)) ++ " /= " ++ (show (length ys)))
 

-- Ex 5: compute the average of a list of values of the Fractional
-- class.
--
-- Hint! since Fractional is a subclass of Num, you have all
-- arithmetic operations available
--
-- Hint! you can use the function fromIntegral to convert the list
-- length to a Fractional

average :: Fractional a => [a] -> a
average [] = 0
average xs = (foldr (+) 0 xs) / fromIntegral (length xs)

-- Ex 6: define an Eq instance for the type Foo below.

data Foo = Bar | Quux | Xyzzy
  deriving Show

instance Eq Foo where
  x == y = fooEq x y
    where 
      fooEq Bar Bar = True
      fooEq Quux Quux = True
      fooEq Xyzzy Xyzzy = True
      fooEq _ _ = False

-- Ex 7: implement an Ord instance for Foo so that Quux < Bar < Xyzzy

instance Ord Foo where
  compare x y
    | x == y = EQ
  compare Quux Bar = LT
  compare Bar Xyzzy = LT
  compare Quux Xyzzy = LT
  compare _ _ = GT

-- Ex 8: here is a type for a 3d vector. Implement an Eq instance for it.

data Vector = Vector Integer Integer Integer
  deriving Show

instance Eq Vector where
 (Vector a b c) == (Vector x y z) =  a == x && b == y && c == z
    

-- Ex 9: implementa Num instance for Vector such that all the
-- arithmetic operations work componentwise.
--
-- You should probably check the docs for which methods Num has!
--
-- Examples:
--
-- Vector 1 2 3 + Vector 0 1 1 ==> Vector 1 3 4
-- Vector 1 2 3 * Vector 0 1 2 ==> Vector 0 2 6
-- abs (Vector (-1) 2 (-3))    ==> Vector 1 2 3
-- signum (Vector (-1) 2 (-3)) ==> Vector (-1) 1 (-1)

vbinop :: (Integer -> Integer -> Integer) -> Vector -> Vector -> Vector
vbinop op (Vector x1 y1 z1) (Vector x2 y2 z2) =
    Vector (op x1 x2) (op y1 y2) (op z1 z2)

vunaryop :: (Integer -> Integer) -> Vector -> Vector
vunaryop  f (Vector x y z) = Vector (f x) (f y) (f z)

instance Num Vector where
   v1 + v2 = vbinop (+) v1 v2
   v1 * v2 = vbinop (*) v1 v2
   v1 - v2 = vbinop (-) v1 v2
   negate  = vunaryop negate
   abs     = vunaryop abs
   signum  = vunaryop signum
   fromInteger n = Vector n n n

-- Ex 10: compute how many times each value in the list occurs. Return
-- the frequencies as a list of (frequency,value) pairs.
--
-- Hint! feel free to use functions from Data.List
--
-- Example:
-- freqs [False,False,False,True]
--   ==> [(3,False),(1,True)]

freqs :: Eq a => [a] -> [(Int,a)]
freqs xs = map f (nub xs)
    where
      f y = (length $ filter (== y) xs, y)
   

-- Ex 11: implement an Eq instance for the following binary tree type

data ITree = ILeaf | INode Int ITree ITree
  deriving Show

instance Eq ITree where
  (==) = teq

teq :: ITree -> ITree -> Bool
teg ILeaf ILeaf = True
teq (INode a t1 t2) (INode b s1 s2) =
        a == b && t1 == s1  && t2 == s2
teq _ _ = False

-- Ex 12: here is a list type parameterized over the type it contains.
-- Implement an instance "Eq a => Eq (List a)" that compares elements
-- of the lists.

data List a = Empty | LNode a (List a)
  deriving Show

instance Eq a => Eq (List a) where
  (==) = leq

leq :: Eq a => List a -> List a -> Bool
leq Empty Empty = True
leq (LNode x1 l1) (LNode x2 l2) = x1 == x2 && l1 == l2
leq _ _ = False

-- Ex 13: start by reading a bit about Functors. A Functor is a thing
-- you can "map" over, e.g. lists, Maybes.
--
-- Implement the function incrementAll that takes a functorial value
-- and increments each number inside by one.
--
-- Examples:
--   incrementAll [1,2,3]     ==>  [2,3,4]
--   incrementAll (Just 3.0)  ==>  Just 4.0

incrementAll :: (Functor f, Num n) => f n -> f n
incrementAll  = fmap (+1)

-- Ex 14: below you'll find a type Result that works a bit like Maybe,
-- but there are two different types of "Nothings": one with and one
-- without an error description.
--
-- Implement the instance Functor Result

data Result a = MkResult a | NoResult | Failure String
  deriving (Show,Eq)

instance Functor Result where
   fmap = mmap
mmap  f (MkResult x) = MkResult (f x)
mmap _ NoResult = NoResult
mmap _ (Failure s) = Failure s

-- Ex 15: Implement the instance Functor List (for the datatype List
-- from ex 11)

instance Functor List where
    fmap = lmap
lmap _  Empty = Empty
lmap f  (LNode a lst) = LNode (f a) (lmap f lst)

-- Ex 16: Fun a is a type that wraps a function Int -> a. Implement a
-- Functor instance for it.
--
-- Figuring out what the Functor instance should do is most of the
-- puzzle.

data Fun a = Fun (Int -> a)

runFun :: Fun a -> Int -> a
runFun (Fun f) x = f x

instance Functor Fun where
   fmap g (Fun f) = Fun $ g.f

-- Ex 17: this and the next exercise serve as an introduction for the
-- next week.
--
-- The module System.Random has the typeclass RandomGen that
-- represents a random generator. The class Random is for values that
-- can be randomly generated by RandomGen.
--
-- The relevant function in System.Random is
--   random :: (Random a, RandomGen g) => g -> (a, g)
-- that takes a random generator and returns a random value, and the
-- new state of the generator (remember purity!)
--
-- Implement the function threeRandom that generates three random
-- values. You don't need to return the final state of the random
-- generator (as you can see from the return type).
--
-- NB! if you use the same generator multiple times, you get the same
-- output. Remember to use the new generator returned by random.
--
-- NB! the easiest way to get a RandomGen value is the function
-- mkStdGen that takes a seed and returns a random generator.
--
-- Examples:
--  *W5> threeRandom (mkStdGen 1) :: (Int,Int,Int)
--  (7917908265643496962,-1017158127812413512,-1196564839808993555)
--  *W5> threeRandom (mkStdGen 2) :: (Bool,Bool,Bool)
--  (True,True,False)

threeRandom :: (Random a, RandomGen g) => g -> (a,a,a)
threeRandom g = (x,y,z)
   where
     (x, g1) = random g
     (y, g2) = random g1
     (z, _)  = random g2

-- Ex 18: given a Tree (same type as on Week 3), randomize the
-- contents of the tree.
--
-- That is, you get a RandomGen and a Tree, and you should return a
-- Tree with the same shape, but random values in the Nodes.
--
-- This time you should also return the final state of the RandomGen
--
-- Hint! the recursive solution is straightforward, but requires
-- careful threading of the RandomGen versions.
--
-- Examples:
--  *W5> randomizeTree (Node 0 (Node 0 Leaf Leaf) Leaf) (mkStdGen 1)  :: (Tree Char, StdGen)
--  (Node '\603808' (Node '\629073' Leaf Leaf) Leaf,1054756829 1655838864)
--  *W5> randomizeTree (Node True Leaf Leaf) (mkStdGen 2)  :: (Tree Int, StdGen)
--  (Node (-2493721835987381530) Leaf Leaf,1891679732 2103410263)


data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

randomizeTree :: (Random a, RandomGen g) => Tree b -> g -> (Tree a,g)
randomizeTree Leaf g = (Leaf, g)
randomizeTree (Node x t1 t2) g  = (Node y t3 t4, g3)
   where 
    (y, g1) = random g
    (t3, g2) = randomizeTree t1 g1
    (t4, g3) = randomizeTree t2 g2
