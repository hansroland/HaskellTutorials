-- ----------------------------------------------------------------------------
-- Name.hs - ProtoHaskell Compiler
-- ----------------------------------------------------------------------------
--
-- See: See: http://dev.stephendiehl.com/fun/
--
--
-- ----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}


module Name (
     -- Name types
     Name(..),
     Named (getName),

     -- Name conversion / renaming
     unName,
     prefix, 

     -- Name supplies
     letters,
     genNames
     ) where


import Data.String
import Data.Monoid
-- import Data.Hashable
import Control.Monad

import GHC.Generics

-- | 
data Name
    = Gen String Integer
    | Name String
    deriving (Eq, Ord, Show, Read, Generic)

-- instance Hashable Name where

-- instance IsString Name where
--    fromString = Name

prefix :: String -> Name -> Name
prefix p (Gen nm i) = Gen (p <> nm) i
prefix p (Name nm) = Name (p <> nm)

-- unName :: isString a => Name -> a
unName :: Name -> String
unName (Name s)  = fromString s
unName (Gen s i) = fromString (s ++ show i)

-- | Generate an infinite list of string with variable names 
--    ["a".."b", "aa", "ab".."zz", "aaa", "aab".."zzzz", "aaaa".."zzzz"...]
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- | Generate an infinite list of names
genNames :: [Name]
genNames = Prelude.zipWith Gen letters [0..]

class Named a where
    getName :: a -> Name


