-- --------------------------------------------------------------------
-- accelerate00.hs
-- --------------------------------------------------------------------

-- On Ubuntu do
--  sudo apt-get install nvidia-cuda-toolkit

import Data.Array.Accelerate as A

-- Choose one of the following:
import Data.Array.Accelerate.Interpreter
import Data.Array.Accelerate.CUDA

{-
The basic data elements:

  -- Shapes for arrays 
  data Z = Z
  type DIM0 = Z
  type DIM1 = DIM0 :. Int
  type DIM2 = DIM1 :. Inty
  -- Note: the type synonyms DIMx do NOT take a type variable!!

  -- Some useful types for Arrays
  type Scalar e = Array DIM0 e
  type Vector e = Array DIM1 e

  -- Special types for accelerate computations:
  Exp a - an accelerate computation delivering a single value
  Acc a - an accelerate computation delivering an array

  -- Special type classes for types accelerate can process
  Elt - class of types that may be array elements
      - Int, Int8, Int16, Int32, Int64
      - Word8, Word16, Word32 Word64 types
      - Float, Double
      - Some Cxxx
      - Arrays are NOT instances of Elt -> no nested arrays!!

   Arrays - 

   Shapes - See above
  
-}

-- -------------------------------------------------------------------
-- Operations to use Accelerate arrays in the Haskell world
-- -------------------------------------------------------------------

-- create accelerate arrays with fromList
-- Note: this creates an array in Haskell memory
fromList (Z:.10) [1..10] :: Vector Int

-- Build a 2 dim array
let arr = fromList (Z:.3:.5) [1..] :: Array DIM2 Int

-- Access a single element with a given index. Note first array, then index
indexArray arr (Z:.2:.1)

-- Create a normal list from an array
toList :: Array sh e -> [e]

-- Arrays of tuples are possible:
fromList (Z:.2:.3) (Prelude.zip [1..] [1..]) :: Array DIM2 (Int, Int)

-- Get the shape from an array
arrayShape :: Shape sh => Array sh e -> sh

-- Get the dimensions of a shape
arrayDim Z  -- -> 0

arrayDim (arrayShape arr)   -- -> 2


-- ----------------------------------------------------------------------
-- Running computations really in the GPU
-- ----------------------------------------------------------------------

-- Basic functions:
-- use :: Arrays a => a -> Acc a      -- inject a Haskell array into GPU
-- run :: Array a => Acc a -> a       -- compute in GPU and return Haskell value

run $ A.map (+1) (use arr)

run $ A.map(^2) (use arr)

-- Create a scalar array
-- unit :: Elt e => Exp e -> Acc (Scalar e)
run $ unit (3::Exp Int)

-- extract the value from a scalar array
-- the :: Elt e => Acc (Scalar e) -> Exp e

-- Support funcions to create shape elemets

index1 :: Elt i => Exp i -> Exp (Z:. i)
index1 :: Elt i => Exp i -> Exp DIM1 i

index2 :: Exp Int -> Exp Int -> Exp DIM2 

-- Support functions to pack and unpack Exp's
unlift :: Exp DIMx -> DIMx

lift :: 

-- ---------------------------------------------------------------
-- Create Arrays inside Acc (without copyint to GPU)
-- ---------------------------------------------------------------

-- fill - create an array with all the same elements
-- fill :: (Shape sh, Elt e) => Exp sh -> Exp e -> Acc (Array sh e)
run $ fill (index1 10) (constant 5) :: (Array DIM1 Int)

-- Note: without run fill give an Acc:  
fill (index1 10) (constant 5) :: (Acc (Array DIM1 Int))

-- enumFromN create an array from a sequence with step 1
run $ enumFromN (index1 10) 5 :: Array DIM1 Int
run $ enumFromN (index1 10) 5 :: Array DIM1 Double


--enumFromStepN - create an array from a sequence with a specified increment 
run $ enumFromStepN (index2 3 5) 15 (-1) :: Array DIM2 Int

-- generate - create an array with a generaing funcion
-- generate :: (Shape ix, Elt a)
--          => Exp ix -> ( Exp ix > Exp a) -> Acc (Array ix a)

run $ generate (index2 3 5) (\ix -> let Z:.y:.x = unlift ix in x+y) :: Array DIM2 Int
-- Note: the type of the lambda function is: Exp DIM2 -> Exp Int
--       ix has type Exp DIM2


-- --------------------------------------------------------------------
-- Extraction of subarrays
-- --------------------------------------------------------------------
slice :: ???


-- ---------------------------------------------------------------------
-- Processing of Arrays in the GPU
-- ---------------------------------------------------------------------
-- Apply the given unary function element-wise to the given array
map :: (Shape ix, Elt a, Elt b) =>
       (Exp a -> Exp b) -> Acc (Array ix a) -> Acc (Array ix b)

-- Apply the given binary function element-wise to the two arrays
zipWith :: (Shape ix, Elt a, Elt b, Elt c) =>
       (Exp a -> Exp b -> Exp c) -> Acc (Array ix a) -> Acc (Array ix b)
       -> Acc (Array ix c)

-- Reduction of the innermost dimension of an array of arbitary rank
-- Note: the first argument needs to be an associative function to get
--       efficient parallel implementation.


run $ A.fold (+) 0 (use arr)

