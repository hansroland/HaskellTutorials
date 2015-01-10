-- -----------------------------------------------------------------------------
-- Monad.hs - Compiler monad for ProtoHaskell
-- -----------------------------------------------------------------------------
--
-- See:
--   http://dev.stephendiehl.com/fun/
--
--
-- -----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monad (
    {-
    -- Compiler driver
    CompilerM,
    runCompilerM,

    -- Compiler state
    CompilerState(..),

    --
    -}
    ) where

import Data.Monoid
import qualified Data.Text.Lazy as L

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except

import qualified Flags
import qualified Frontend as Syn

