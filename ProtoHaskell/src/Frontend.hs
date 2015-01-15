-- ----------------------------------------------------------------------------
-- Frontend.hs - ProtoHaskell Compiler
-- ----------------------------------------------------------------------------
--
-- Fontend contains the data types and classes for language elements
--         of ProtoHaskell
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

     -- * Constructors
     mkEApp,
     mkELam,
     mkPair,
     mkList,
     mkIf,

     -- * Deconstructors
     viewApp,
     viewLam,
     viewVars,

     -- * Free variables
    freeVars,
    allVars,
    boundVars,
    occursIn,
    fconsDecl,

    -- * Declaration manipulation
    sdef,
    sname,
    sget,
    slookup,
    ssingleton,
    globals,

    -- * Declaration grouping
    groupToplevel,
    groupDecls,

    -- * Pattern manipulation
    pvars,
    freePvs
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
    = FunDecl BindGroup                   -- functions: f x = x + 1
    | TypeDecl Type                       -- types      f :: Int -> Int
    | DataDecl Constr [Name] [ConDecl]    -- data defs: data T where {..}
    | ClassDecl [Pred] Name [Name] [Decl] -- class      class (P) => where {..}
    | InstDecl  [Pred] Name Type [Decl]   -- instance declarations
    | FixityDecl FixitySpec               -- infix 1 {...}
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


-- | Extract the desugared bind group name.
sname :: Decl -> Name
sname (FunDecl (BindGroup name [Match _ _] _ _)) = name
sname (DataDecl name _ _) = name
sname (ClassDecl _ name _ _) = name
sname (InstDecl _ name _ _) = name
sname _ = error "Bind group is not in desugared form"

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

-- | Extract a set of the named constructor used in a type declaration
fconsDecl :: Decl -> Set.Set Name
fconsDecl (DataDecl _ _ xs) = Set.unions $ fmap fconsConDecl xs
fconsDecl _ = Set.empty

-- | The global function names
globals :: Module -> [Name]
globals (Module _ decls) = fmap sname decls
  where
    sname :: Decl -> Name
    sname (FunDecl (BindGroup name _ _ _)) = name
    sname (DataDecl name _ _) = name
    sname (ClassDecl _ name _ _) = name
    sname (InstDecl _ name _ _) = name
    sname _ = error "Bind group is not in desugared form"

-------------------------------------------------------------------------------
-- Grouping
-------------------------------------------------------------------------------

groupToplevel :: Module -> Module
groupToplevel (Module nm decls) = Module nm $ mconcat [clsub, icls, datas, funs]
  where
    funs =  groupDecls [e | e@FunDecl{} <- decls]
    datas = [e | e@DataDecl{} <- decls]
    clsub = [e | e@ClassDecl{} <- decls]
    icls  = [e | e@InstDecl{} <- decls]

groupBindings :: [BindGroup] -> [BindGroup]
groupBindings = fmap joinBindings . groupBy ((==) `on` _matchName)

joinBindings :: [BindGroup] -> BindGroup
joinBindings xs@(x:_) =
    BindGroup (_matchName x) (concatMap _matchPats xs) (_matchType x) (concatMap _matchWhere xs)
joinBindings [] = error "empty binding group"

groupDecls :: [Decl] -> [Decl]
groupDecls decls = fmap FunDecl $ groupBindings (concatMap fgroup decls)

-- --------------------------------------------------------------------
-- Traversal
-- --------------------------------------------------------------------

descend :: (Expr -> Expr) -> Expr -> Expr
descend f ex = runIdentity (descendM (return . f) ex)

-- | General function for bottom up traversals and rewrites in a monadic context
descendM :: (Monad m, Applicative m) => (Expr -> m Expr) -> Expr -> m Expr
descendM f e = case e of
    EApp a b   -> EApp  <$> descendM f a <*> descendM f b
    EVar a     -> EVar  <$> pure a
    ELam a b   -> ELam  <$> pure a <*> descendM f b
    ELit n     -> ELit  <$> pure n
    ELet n a b -> ELet  <$> pure n <*> descendM f a <*> descendM f b
    EIf a b c  -> EIf   <$> descendM f a <*> descendM f b <*> descendM f c
    ECase a xs -> ECase <$> f a <*> traverse (descendCaseM f) xs
    EAnn a t   -> EAnn  <$> descendM f a <*> pure t
    --EDo [Stmt] is missing !!!
    EFail      -> pure EFail
-- Notes: Leave nodes are just packed with pure
--        Branch nodes are descended further, other node types like 'Match'
--        need other descending functions like descendCaseM


descendCaseM :: (Monad m, Applicative m) => (Expr -> m Expr) -> Match -> m Match
descendCaseM f e = case e of
    Match ps a -> Match <$> pure ps <*> descendM f a


compose :: (Expr -> Expr)
    -> (Expr -> Expr)
    -> (Expr -> Expr)
compose f g = descend (f . g)

composeM :: (Applicative m, Monad m)
    => (Expr -> m Expr)
    -> (Expr -> m Expr)
    -> (Expr -> m Expr)
composeM f g = descendM (f <=< g)

-------------------------------------------------------------------------------
-- Class AllVar
-------------------------------------------------------------------------------

-- | Class AllVar: Return a set of all variable names from something
class AllVars a where
    allVars :: a -> Set.Set Name

-- | If a is an instance of AllVar also [a] is na instance of AllVars
instance AllVars a => AllVars [a] where
    allVars = Set.unions . fmap allVars

--
instance AllVars Pattern where
  allVars pt = case pt of
    PVar n -> Set.singleton n
    PCon _ ps -> Set.unions $ fmap allVars ps
    PLit _ -> Set.empty
    PWild -> Set.empty

instance AllVars Match where
  allVars ex = case ex of
    Match pats rhs -> allVars rhs


instance AllVars Expr where
  allVars ex = case ex of
    EVar x     -> Set.singleton x
    ELet n v e -> Set.unions [Set.singleton n, allVars v, allVars e]
    ELam _ e   -> allVars e
    EApp a b   -> allVars a `Set.union` allVars b
    ECase n as -> allVars n `Set.union` Set.unions (fmap allVars as)
    ELit _     -> Set.empty
    EIf c x y  -> Set.unions [allVars c, allVars x, allVars y]
    EAnn x _   -> allVars x
    EFail      -> Set.empty

instance AllVars Decl where
    allVars (FunDecl bg) = allVars bg
    allVars (DataDecl {}) = Set.empty
    allVars (TypeDecl {}) = Set.empty
    allVars (ClassDecl {}) = Set.empty
    allVars (InstDecl {}) = Set.empty
    allVars (FixityDecl {})= Set.empty

instance AllVars BindGroup where
    allVars (BindGroup _ pats _ _) = Set.unions (fmap allVars pats)

-------------------------------------------------------------------------------
-- Class FreeVar
-------------------------------------------------------------------------------

-- | Instances return a set of free variables.
class FreeVars a where
    freeVars :: a -> Set.Set Name

-- | Lists of FreeVar instances are FreeVars too
instance FreeVars a => FreeVars [a] where
    freeVars = Set.unions . fmap freeVars

instance FreeVars Expr where
  freeVars ex = case ex of
    ELam n x -> freeVars x Set.\\ Set.singleton n
    ELet n v e -> (freeVars e Set.\\ Set.singleton n) `Set.union` (freeVars v)
    EApp f xs -> freeVars f `Set.union` freeVars xs
    ECase e m -> freeVars e `Set.union` Set.unions (fmap freeVars m)
    EDo xs -> Set.unions (fmap freeVars xs)
    EVar n -> Set.singleton n
    ELit _ -> Set.empty
    EIf c x y -> freeVars c `Set.union` freeVars x `Set.union` freeVars y
    EAnn x _ -> freeVars x
    EFail -> Set.empty

instance FreeVars Match where
  freeVars ex = case ex of
    Match pats rhs -> freeVars rhs Set.\\ Set.unions (fmap allVars pats)


instance FreeVars Decl where
    freeVars (FunDecl bg) = freeVars bg
    freeVars (DataDecl {}) = Set.empty
    freeVars (TypeDecl {}) = Set.empty
    freeVars (ClassDecl {}) = Set.empty
    freeVars (InstDecl {}) = Set.empty
    freeVars (FixityDecl {}) = Set.empty

instance FreeVars Stmt where
  freeVars ex = case ex of
    Generator pat x -> freeVars x Set.\\ (allVars pat)
    Qualifier x -> freeVars x


instance FreeVars BindGroup where
  freeVars (BindGroup _ pats _ _) = Set.unions (fmap freeVars pats)


-- | Check, whether a variable name occurs in a value
occursIn :: AllVars a => Name -> a -> Bool
occursIn name ex = name `Set.member` (allVars ex)

-- | Return a set with all the bound variables
boundVars :: (FreeVars a, AllVars a) => a -> Set.Set Name
boundVars ex = (allVars ex) `Set.difference` (freeVars ex)


-- free pattern variables
freePvs :: Pattern -> [Name]
freePvs (PVar a) = [a]
freePvs (PCon _ b) = concatMap freePvs b
freePvs (PLit _) = []
freePvs (PWild) = []

-- ----------------------------------------------------------------------------
-- Constants for the constructors of the built in syntax
-- ----------------------------------------------------------------------------

_paircon :: Name
_paircon = "Pair" 

_conscon :: Name
_conscon = "Cons"

_nilcon :: Name
_nilcon = "Nil"

mkEApp :: Expr -> [Expr] -> Expr
mkEApp = foldl' EApp

mkELam :: Expr -> [Name] -> Expr
mkELam = foldr ELam

mkPair :: [Expr] -> Expr
mkPair = foldr1 pair
  where
    pair x y = mkEApp (EVar _paircon) [x,y]

mkList :: [Expr] -> Expr
mkList = foldr cons nil
  where
    cons x y = mkEApp (EVar _conscon) [x,y]
    nil = EVar _nilcon

mkIf :: Expr -> Expr
mkIf (EIf c x y) =
   ECase c [
       Match [PCon "True" []] x,
       Match [PCon "False" []] y
   ]
mkIf x = x

-------------------------------------------------------------------------------
-- Deconstructors
-------------------------------------------------------------------------------
viewVars :: Expr -> [Name]
viewVars (ELam n a) = n : viewVars a
viewVars _ = []

viewLam :: Expr -> Expr
viewLam (ELam _ a) = viewLam a
viewLam x = x

viewApp :: Expr -> (Expr, [Expr])
viewApp = go []
  where
    go !xs (EApp a b) = go (b : xs) a
    go xs f = (f, xs)

