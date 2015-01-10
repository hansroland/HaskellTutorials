-- ----------------------------------------------------------------------------
-- Frontend.hs - ProtoHaskell Compiler
-- ----------------------------------------------------------------------------
--
-- See: http://dev.stephendiehl.com/fun/
--
--
-- ----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Frontend (

   ) where

import Prelude hiding(foldr, foldr1, concatMap)

import Name
import Type

import Data.Monoid
import Data.Foldable
import Data.Function (on)
import Data.List (groupBy)
import Data.Traversable
import qualified Data.Set as Set

import Control.Applicative
import Control.Monad.Identity

import GHC.Word (Word8)

-- ----------------------------------------------------------------------------
-- Surface Language
-- ----------------------------------------------------------------------------

type Constr = Name

data Expr
    = EApp  Expr Expr        -- a b
    | EVar  Name             -- x
    | ELam  Name Expr        -- \\x.y
    | ELit  Literal
    | ELet  Name Expr Expr   -- let x = y in z
    | EIf   Expr Expr Expr   -- if x then tr else fa
    | ECase Expr [Match]     -- case x of { p -> e; ... }
    | EAnn  Expr Type        -- x :: Int
    | EDo   [Stmt]           -- do { ... }
    | EFail                  -- pattern match
    deriving (Eq, Show)
    

-- data type to model a line in a do group
data Stmt
    = Generator Pattern Expr  -- pat <- exp
    | Qualifier Expr          -- exp (eg a let expression)
    deriving (Eq, Show)

-- | data type for literals
data Literal
    = LitInt Int              -- 1
    | LitChar Char            -- 'a'
    | LitString [Word8]       -- A primitive C-style string, type Addr#
   deriving (Eq, Ord, Show)

-- data type patterns
data Pattern
    = PVar Name               --  x
    | PCon Constr [Pattern]   --  C x y
    | PLit Literal            -- ^ 3
    | PWild                   -- ^ _
    deriving (Eq, Show)


data Match = Match
    { _matchPat :: [Pattern]
    , _matchBody :: Expr
    } deriving (Eq, Show)







