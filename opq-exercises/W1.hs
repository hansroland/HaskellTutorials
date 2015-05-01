module W1 where

-- See: https://github.com/opqdonut/haskell-exercises

-- Week 1:
--   * defining functions
--   * basic expressions
--   * pattern matching
--   * recursion

-- Ex 1: define variables one, two and three. They should all have
-- type Int and values 1, 2 and 3. This exercise has no tests.

one :: Int
one = 1

two :: Int
two = 2

three :: Int
three = 3

-- Ex 2: define the function double of type Integer->Integer. Double
-- should take one argument and return it multiplied by two.

double :: Integer -> Integer
double x = x + x

-- Ex 3: define the function quadruple that uses the function double
-- from the previous exercise to return its argument multiplied by
-- four.

quadruple :: Integer -> Integer
quadruple x = double $ double x

-- Ex 4: define the function poly2. It should take four arguments of
-- type Double, a, b, c, and x and return a*x^2+b*x+c. Give poly2 a
-- type signature, i.e. poly2 :: something.

poly2 :: Double -> Double -> Double -> Double -> Double
poly2 a b c x = a*x^2+b*x+c

-- Ex 5: define the function eeny that returns "eeny" for even inputs
-- and "meeny" for odd inputs.
--
-- Ps. have a look at the built in function "even"

eeny :: Integer -> String
eeny  x 
    | even x = "eeny"
    | otherwise = "meeny"

-- Ex 6: fizzbuzz! Define the a function fizzbuzz that returns "Fizz"
-- for numbers divisible by 3, "Buzz" for numbers divisible by 5, and
-- "FizzBuzz" for numbers divisible by both. For other numbers it
-- returns the empty string.
--
-- You can use the function mod to compute modulo.

fizzbuzz x 
   | x `mod` 15 == 0 = "FizzBuzz"
   | x `mod` 5 == 0 = "Buzz"
   | x `mod` 3 == 0 = "Fizz"
   | otherwise = ""

-- Ex 7: define a function isZero that returns True if it is given an
-- Integer that is 0, and False otherwise. Give isZero a type signature.
--
-- Use pattern matching! Don't use comparisons!
--
-- Ps. the type of booleans in haskell is Bool

isZero :: Integer -> Bool
isZero 0 = True
isZero _ = False

-- Ex 8: implement using recursion a function sumTo such that
--   sumTo n
-- computes the sum 1+2+...+n

sumTo :: Integer -> Integer
sumTo 0 = 0
sumTo n = n + (sumTo (n - 1))

-- Ex 9: power n k should compute n to the power k (i.e. n^k)
-- Use recursion.

power :: Integer -> Integer -> Integer
power n 0 = 1
power n k = n * power n (k - 1)

-- Ex 10: ilog2 n should be the number of times you can halve the
-- integer n (rounding down) before you get 1.
--
-- Use recursion to define ilog2. Use the function "div" for integer
-- division.

ilog2 :: Integer -> Integer
ilog2 n = z
  where 
     (_,z) = ilog2a  n  0 
     ilog2a 1  c  = (1, c)
     ilog2a x  c  = ilog2a (div x 2)  (c + 1)

-- Ex 11: compute binomial coefficients using recursion. Binomial
-- coefficients are defined by the following equations:
--
--   B(n,k) = B(n-1,k) + B(n-1,k-1)
--   B(n,0) = 1
--   B(0,k) = 0, when k>0
--
-- Hint! pattern matching is your friend.

binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = (binomial (n - 1) k) + (binomial (n - 1) (k - 1))

-- Ex 12: The tribonacci numbers are defined by the equations
--
--   T(1) = 1
--   T(2) = 1
--   T(3) = 2
--   T(n+1) = T(n)+T(n-1)+T(n-2)
--
-- Implement an efficient, linear time, recursive function that
-- computes T(n). You'll probably want to define a helper function.

tribonacci :: Integer -> Integer
tribonacci n =  getAt trips n
   where
      trips = 1 : 1 : 2 : zipWith3 (add3) trips (tail trips) (tail (tail trips))
      add3 x y z = x + y + z
      getAt :: [a] -> Integer -> a
      getAt lst 1 = head lst
      getAt lst n = getAt (tail lst) (n- 1)



-- Ex 13: implement the euclidean algorithm for finding the greatest
-- common divisor: http://en.wikipedia.org/wiki/Euclidean_algorithm

myGcd :: Integer -> Integer -> Integer
myGcd x y 
   | x < y = myGcd y x
myGcd x 0 = x
myGcd x y = myGcd y (rem x y)

-- Ex 14: The Haskell Prelude (standard library) defines the type
-- Ordering with values LT, GT and EQ. You try out Ordering by
-- evaluating the following expressions:
--
--   compare 3 4
--   compare 4 3
--   compare 0 0
--   compare "Hei" "Moi"
--   compare True False
--
-- Your task is to implement the function funnyCompare, that orders
-- integers in the following way:
--
-- 1. All even numbers come before odd numbers
-- 2. Within even and odd numbers the ordering is normal

funnyCompare :: Int -> Int -> Ordering
funnyCompare x y
   | (even x) && (even y) = compare x y
   | (odd x)  && (odd y)  = compare x y
   | (even x) && (odd y)  = LT
   | (odd x) && (even y)  = GT

-- Ex 15: Implement the function funnyMin that returns the minimum of
-- its two arguments, according to the ordering implemented by
-- funnyCompare.
--
-- Use pattern matching on the Ordering value returned by
-- funnyCompare. To do this, you need to either use the case-of
-- expression or define a helper function.

funnyMin :: Int -> Int -> Int
funnyMin x y =
   case funnyCompare x y of
     LT -> x
     GT -> y
     EQ -> x

-- Ex 16: implement the recursive function pyramid that returns
-- strings like this:
--
-- pyramid 0 ==> "0"
-- pyramid 1 ==> "0,1,0"
-- pyramid 2 ==> "0,1,2,1,0"
-- pyramid 3 ==> "0,1,2,3,2,1,0"
--
-- Hints:
-- * you can glue strings together with the operator ++
-- * the function show transforms a number into a string
-- * you'll need a (recursive) helper function

pyramid :: Integer -> String
pyramid n = wrap (n-1) (show n)
   where
      wrap (-1) s = s
      wrap n s = wrap (n - 1) ( (show n) ++ "," ++ s ++ "," ++ (show n))

-- Ex 17: implement the function smallestDivisor that returns the
-- smallest number (greater than 1) that divides the given number.
--
-- That is, when
--   smallestDivisor n ==> k
-- we have
--   n = t*k
-- for some t.
--
-- Ps. your function doesn't need to work for inputs 0 and 1, but
-- remember this in the next exercise!

smallestDivisor :: Integer -> Integer
smallestDivisor n = head  [k | k <- [2..n] ,  rem n k == 0]

-- Ex 18: implement a function isPrime that checks if the given number
-- is a prime number. Use the function smallestDivisor.
--
-- Ps. 0 and 1 are not prime numbers

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = smallestDivisor n == n

-- Ex 19: implement a function nextPrime that returns the first prime
-- number that comes after the given number. Use the function isPrime
-- you just defined.

nextPrime :: Integer -> Integer
nextPrime n = head [k | k <- [(n+1)..] , isPrime k]

