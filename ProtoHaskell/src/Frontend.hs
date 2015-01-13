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
     -- * Frontend AST
     Expr(..),
     Decl(..),
     Match(..),
     BindGroup(..),
     Pattern(..),
     ConDecl(..),
     Module(..),
     Stmt(..),
     Literal(..),
     Fixity(..),
     FixitySpec(..),
     Assoc(..),
     Constr,
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


-- | 
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
    = FunDecl BindGroup                 -- functions: f x = x + 1
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

-- | Module - A module has a name and a list of declarations
data Module = Module Name [Decl]
   deriving (Eq, Show)

-- --------------------------------------------------------------------------
-- Extraction
-- --------------------------------------------------------------------------

-- Get the binding groups associated with a definition
fgroup :: Decl -> [BindGroup]
fgroup (FunDecl xs) = [xs]
fgroup _ = []

-- | Extract pattern variables.
pvars :: [Pattern] -> [Name]
pvars ps = [a | PVar a <- ps]

-- | Lookup a toplevel declaration by name.
slookup :: String -> Module -> Maybe Decl
slookup nm (Module _ decls) =
    case decls' of
        [] -> Nothing
        (x:_) -> Just x
    where
      decls' = [d | d@(FunDecl (BindGroup name _ _ _)) <- decls, name == Name nm]

-- | Extract a function declaration by name.
sget :: Name -> [Decl] -> Maybe (Name, Maybe Type, Expr)
sget nm decls =
    case decls' of
         [] -> Nothing
         (x:_) -> Just (sdef x)
    where
        decls' = [d | d@(FunDecl (BindGroup name _ _ _)) <- decls, name == nm]

-- Singleton named toplevel declaration.
ssingleton :: Name -> Expr -> Decl
ssingleton nm ex = FunDecl (BindGroup nm [Match [] ex] Nothing [])

-- | Extract the desugared bind group.
sdef :: Decl -> (Name, Maybe Type, Expr)
sdef (FunDecl (BindGroup name [Match [] rhs] tysig _)) = (name, tysig, rhs)
sdef _ = error "Bind group is not in desugared form"


-- | Extract a set of the named constructor used in a type
fcons :: Type -> Set.Set Name
fcons (TCon (AlgTyCon n)) = Set.singleton n
fcons (TCon {}) = Set.empty
fcons (TVar {}) = Set.empty
fcons (t1 `TArr` t2) = fcons t1 `Set.union` fcons t2
fcons (t1 `TApp` t2) = fcons t1 `Set.union` fcons t2

fconsConDecl :: ConDecl -> Set.Set Name
fconsConDecl (ConDecl _ (TForall _ _ ty)) = fcons ty
fconsConDecl (RecDecl _ _ (TForall _ _ ty)) = fcons ty






