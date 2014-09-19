-- ----------------------------------------------------------------------
-- Some simple examples
-- ----------------------------------------------------------------------

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter

-- | simple example: The vector dot product

dotp :: Vector Float -> Vector Float -> Acc (Scalar Float)
dotp xs ys = let xs' = use xs
                 ys' = use ys
             in 
             fold (+) 0 (A.zipWith (*) xs' ys')

x1 :: Vector Float
x1 = A.fromList (Z:.3) [2.3, 4.5, (-1.2)]

x2 :: Vector Float
x2 = A.fromList (Z:.3) [2.2, 3.14, 7.6]

-- example: run $ dotp x1 x2

-- extract the real float, without all wrappers
-- head $ toList $ run $ dotp x1 x2


