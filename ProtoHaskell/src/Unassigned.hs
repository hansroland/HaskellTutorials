

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




