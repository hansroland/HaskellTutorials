

import Control.Monad
import Control.Applicative

import Frontend

-- --------------------------------------------------------------------
-- Compiler Monad and State
-- --------------------------------------------------------------------

{-
type CompilerMonad =
  ExceptT Msg
    (StateT CompilerState IO)

data CompilerState = CompilerState
  { _fname    :: Maybe FilePath            -- ^ File path
  , _imports  :: [FilePath]                -- ^ Loaded modules
  , _src      :: Maybe L.Text              -- ^ File source
  , _ast      :: Maybe Syn.Module          -- ^ Frontend AST
  , _tenv     :: Env.Env                   -- ^ Typing environment
  , _kenv     :: Map.Map Name Kind         -- ^ Kind environment
  , _cenv     :: ClassEnv.ClassEnv         -- ^ Typeclass environment
  , _cast     :: Maybe Core.Module         -- ^ Core AST
  , _flags    :: Flags.Flags               -- ^ Compiler flags
  , _venv     :: CoreEval.ValEnv Core.Expr -- ^ Core interpreter environment
  , _denv     :: DataEnv.DataEnv           -- ^ Entity dictionary
  , _clenv    :: ClassEnv.ClassHier        -- ^ Typeclass hierarchy
  } deriving (Eq, Show)

-}

-- --------------------------------------------------------------------
-- Traversal
-- --------------------------------------------------------------------


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

traverse = undefined


