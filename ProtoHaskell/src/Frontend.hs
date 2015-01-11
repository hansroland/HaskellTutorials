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
    

-- | Stmt data type to model a line in a do group
--      The do-notation syntax is written in terms of two constructions,
--      one for monadic binds and the other for monadic statements.
data Stmt
    = Generator Pattern Expr  -- pat <- exp
    | Qualifier Expr          -- exp (eg a let expression)
    deriving (Eq, Show)

-- | Literal data type for the atomic data
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


data BindGroup = BindGroup
    { _matchName  :: Name
    , _matchPats  :: [Match]
    , _matchType  :: Maybe Type
    , _matchWhere :: [[Decl]]
    } deriving (Eq, Show)


data Match = Match
    { _matchPat :: [Pattern]
    , _matchBody :: Expr
    } deriving (Eq, Show)


data ConDecl
    = ConDecl Constr Type
    | RecDecl Constr[(Name, Type)] Type
    deriving (Eq, Show, Ord)

-- | Decl - all possible declarations
data Decl
    = FuncDecl1 BindGroup               -- functions: f x = x + 1
    | TypeDecl Type                     -- types      f :: Int -> Int
    | DataDecl Constr [Name] [ConDecl]  -- data defs: data T where {..}
    | ClassDecl [Pred] Name Type [Decl] -- class      class (P) => where {..}
    | InstDecl  [Pred] Name Type [Decl] -- instance declarations
    | FixityDecl FixitySpec             -- infix 1 {...}
    deriving (Eq, Show)

-- | FixitySepc - Fixity declarations are simply a binding
--      between the operator symbol and the fixity information.
data FixitySpec = FixitySpec
    { fixityFix  :: Fixity
    , fixityName :: String
    } deriving (Eq, Show)

data Assoc = L | R | N
    deriving (Eq, Ord, Show)

data Fixity
    = Infix Assoc Int
    | Prefix Int
    | Postifx Int
    deriving (Eq, Show)

-- | temporary definition for Pred (
data Pred = Pred
    deriving (Show, Eq)

-- | Module - A module has a name and a list of declarations
data Module = Module Name [Decl]
   deriving (Eq, Show)









