-- ------------------------------------------------------------------------
-- Read.hs 
-- ------------------------------------------------------------------------
--
-- See:
-- http://funktionale-programmierung.de/2015/04/15/monaden-reverse-engineering.html
--
-- - Reading binary data with handwritten fuctions and parsers monads
-- - Reading additional data with offset pointers
--
-- ------------------------------------------------------------------------


import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Control.Monad

-- | Just a little function to look at our binary file
test0 :: IO()
test0 = do
   bs <- BS.readFile "parser.dat"
   putStrLn $ show $ BS.unpack bs


-- | a generalized test function for our parsers
testParser :: Show a => (ByteString -> a) -> IO()
testParser pf = do
   bs <- BS.readFile "parser.dat"
   putStrLn $ show $ pf bs


-- | A first parser reading a 8 bit word and two 16-bit words
parser1 :: ByteString -> (Word8, Word16, Word16)
parser1 bs = (byte0, word1, word2)
   where
    byte0 = BS.index bs 0
    word1 = 2^8 * fromIntegral (BS.index bs 1) + fromIntegral (BS.index bs 2)
    word2 = 2^8 * fromIntegral (BS.index bs 3) + fromIntegral (BS.index bs 4)


test1 :: IO()
test1 = testParser parser1

-- ---------------------------------------------------------------------------
-- Make it a little nicer
-- ---------------------------------------------------------------------------

type Index = Int

-- | Read a word at an index
getWord8At :: ByteString -> Index -> Word8
getWord8At = BS.index

getWord16At :: ByteString -> Index -> Word16
getWord16At bs i = 2^8 * fromIntegral b1 + fromIntegral b2
  where
    b1 = getWord8At bs i
    b2 = getWord8At bs $ i + 1

parser2 :: ByteString -> (Word8, Word16, Word16)
parser2 bs = (byte0, word1, word2)
  where
    byte0 = getWord8At bs 0
    word1 = getWord16At bs 1
    word2 = getWord16At bs 3

test2 :: IO()
test2 = testParser parser2

-- ----------------------------------------------------------------------------
-- Next improvment: Avoid specifying indexes on consecutive getWord functions
-- ----------------------------------------------------------------------------
-- Trick: Return the continuation index as a result

getWord8AtI :: ByteString -> Index -> (Word8, Index)
getWord8AtI bs i = (BS.index bs i , i + 1)

getWord16AtI :: ByteString -> Index -> (Word16, Index)
getWord16AtI bs i0 = (2^8 * fromIntegral b1 + fromIntegral b2, i2)
  where
    (b1, i1) = getWord8AtI bs i0
    (b2, i2) = getWord8AtI bs i1

parser3 :: ByteString -> (Word8, Word16, Word16)
parser3 bs = (byte0, word1, word2)
  where
    i0 = 0
    (byte0, i1) = getWord8AtI bs i0
    (word1, i2) = getWord16AtI bs i1
    (word2, i3) = getWord16AtI bs i2

test3 :: IO()
test3 = testParser parser3


-- -------------------------------------------------------------------------
-- Next improvement: Avoid passing around all the indexes
-- -------------------------------------------------------------------------

-- | Factor out common part of type definition from getWord8AtI, getWord16AtI
newtype Parser a = Parser (ByteString -> Index -> (a, Index))

-- | Pattern match on Parser only in one place !! (unwrap)
runParser :: Parser a -> ByteString -> Index -> (a, Index)
runParser (Parser p) = p

-- | evalParser = appyl a 
evalParser :: Parser a -> ByteString -> a
evalParser p bs = fst $ runParser p bs 0


-- Our Parser should be a Applicative 
-- instance Applicative Parser where
--     pure x = Parser (\bs i -> (x,i))
    

-- Our Parser is a Monad !!
instance Monad Parser where
    return x = Parser (\bs i -> (x,i))
    p1 >>= p2 = Parser (\bs i0 -> 
       let (x, i1) = runParser p1 bs i0
           (y, i2) = runParser (p2 x) bs i1
       in (y, i2))


getWord8P :: Parser Word8
getWord8P = Parser (\bs i -> (BS.index bs i, i+1))

getWord16P :: Parser Word16
getWord16P = do
   b1 <- getWord8P
   b2 <- getWord8P
   return (2^8 * fromIntegral b1 + fromIntegral b2)

parser4 :: ByteString -> (Word8, Word16, Word16)
parser4 = evalParser $ do
   byte0 <- getWord8P
   word1 <- getWord16P
   word2 <- getWord16P
   return (byte0, word1, word2)

test4 = testParser parser4

-- ---------------------------------------------------------------------------
-- Application of our binary parser
-- ---------------------------------------------------------------------------

-- Assume the first word-8 entry gives the number of following word-16 entries

getWord16ListP :: Parser [Word16]
getWord16ListP = do
   n <- getWord8P
   entries <- replicateM (fromIntegral n) getWord16P
   return entries

getWord16List :: ByteString -> [Word16]
getWord16List bs = evalParser getWord16ListP bs

testR :: IO()
testR = do
   bs <- BS.readFile "parser.dat"
   putStrLn $ show $ getWord16List bs

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
    let (x, _) = runParser p bs offset
    in (x,i)

indirection :: Parser a -> Parser a
indirection p = do
    offset <- getWord16P
    lookAt (fromIntegral offset) p

parser5 :: ByteString -> ([Word16], [Word16])
parser5 = evalParser $ do
   list1 <- indirection getWord16ListP
   list2 <- indirection getWord16ListP
   return (list1, list2)

test5 :: IO()
test5  = do
   bs <- BS.readFile "pointer.dat"
   putStrLn $ show $ parser5 bs



