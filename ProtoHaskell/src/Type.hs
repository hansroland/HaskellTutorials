-- ----------------------------------------------------------------------------
-- Type.hs - ProtoHaskell Compiler
-- ----------------------------------------------------------------------------
--
-- See: http://dev.stephendiehl.com/fun/
--
--
-- ----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Type (
     -- Types
     Type(..)
     ) where

import Name
import Data.Char
import Data.String
import Data.List (foldl')


data Type
    = TVar TVar
    deriving (Show, Eq, Ord)





-- ----------------------------------------------------------------------------
-- Type Variables
-- ----------------------------------------------------------------------------

data TVar = TV
    {tvName :: Name
    } deriving (Show, Eq, Ord)

-- instance IsString TVar where
--    fromString x = TV (fromString x)
