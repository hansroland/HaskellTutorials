-- ------------------------------------------------------------------------
-- Read.hs 
-- ------------------------------------------------------------------------
--
-- See:
-- http://funktionale-programmierung.de/2015/04/15/monaden-reverse-engineering.html
--
-- - Reading binary data with handwritten fuctions and parsers monads
-- - Reading additional data with offset pointers
-- - Reading Segments
--
-- This is the final code of Read.hs extended with the possibility to name
-- the read segments
-- ------------------------------------------------------------------------


import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Control.Monad

type Index = Int

-- | Segments have a name and a byte range
type Seg = (String, Index, Index)

-- | Factor out common part of type definition from getWord8AtI, getWord16AtI
newtype Parser a = Parser (ByteString -> Index -> (a, [Seg], Index))

-- | Pattern match on Parser only in one place !! (unwrap)
runParser :: Parser a -> ByteString -> Index -> (a, [Seg], Index)
runParser (Parser p) = p

-- | evalParser = appyl a 
evalParser :: Parser a -> ByteString -> (a, [Seg])
evalParser p bs = (a, lst)
   where (a, lst, _) = runParser p bs 0

-- Our Parser should be a Applicative 
-- instance Applicative Parser where
--     pure x = Parser (\bs i -> (x,i))
    

-- Our Parser is a Monad !!
instance Monad Parser where
    return x = Parser (\bs i -> (x, [], i))
    p1 >>= p2 = Parser (\bs i0 -> 
       let (x, s1, i1) = runParser p1 bs i0
           (y, s2, i2) = runParser (p2 x) bs i1
       in (y, s1 ++ s2, i2))


getWord8P :: Parser Word8
getWord8P = Parser (\bs i -> (BS.index bs i, [],  i+1))

getWord16P :: Parser Word16
getWord16P = do
   b1 <- getWord8P
   b2 <- getWord8P
   return (2^8 * fromIntegral b1 + fromIntegral b2)


named :: String -> Parser a -> Parser a
named n p = Parser $ \bs i0 ->
    let (x, segments, i1) = runParser p bs i0
        segments' = (n, i0,i1) : map qualify segments
    in (x,segments',i1)
  where qualify (n', i0, i1) = (n ++ "/" ++ n', i0, i1)

-- ---------------------------------------------------------------------------
-- Application of our binary parser
-- ---------------------------------------------------------------------------

-- Assume the first word-8 entry gives the number of following word-16 entries

getWord16ListP :: Parser [Word16]
getWord16ListP = do
   n <- getWord8P
   entries <- replicateM (fromIntegral n) getWord16P
   return entries

getWord16List :: ByteString -> ([Word16], [Seg])
getWord16List bs = evalParser getWord16ListP bs

-- ---------------------------------------------------------------------------
-- Forward pointers: We have a pointer that points to a later value
-- ---------------------------------------------------------------------------

-- Our test file 
-- Two 16-bit Words, each with the pointers (offsets) to a list
-- every list has an 8 byte count entry and then so many 16-bit entries

-- look at our list
testlist0 :: IO()
testlist0 = do
   bs <- BS.readFile "pointer.dat"
   putStrLn $ show $ BS.unpack bs


-- | return the value at an offset 
lookAt :: Index -> Parser a -> Parser a
lookAt offset p = Parser $ \bs i ->
    let (x, s, _) = runParser p bs offset
    in (x,s,i)

indirection :: Parser a -> Parser a
indirection p = do
    offset <- getWord16P
    lookAt (fromIntegral offset) p

parser6 :: ByteString -> (([Word16], [Word16]),[Seg])
parser6 = evalParser $ named "Header" $ do
   list1 <- indirection $ named "Liste 1" getWord16ListP
   list2 <- indirection $ named "Liste 2" getWord16ListP
   return (list1, list2)


test6 :: IO()
test6  = do
   bs <- BS.readFile "pointer.dat"
   putStrLn $ show $ parser6 bs
