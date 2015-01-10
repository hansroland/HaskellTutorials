-- ----------------------------------------------------------------------------
-- Flags.hs - The flags of the ProtoHaskell compiler
-- ----------------------------------------------------------------------------

module Flags (
  -- * Compiler flags
  Flag(..),
  Flags,

  -- * Setting / Unsetting
  isSet,
  set,
  unset,

  -- Command line switches
  flagOpts,
  flagFor
  ) where

import qualified Data.Set as S
import Control.Monad (msum)
import Data.List (isPrefixOf)


-- | Data type Flag with all the possible options
data Flag
  = DumpC
  | DumpLLVM               -- -ddump-llvm
  | DumpASM                -- -ddump-asm
  | DumpParsed             -- -ddump-parsed
  | DumpDesugar            -- -ddump-ds
  | DumpInfer              -- -ddump-infer
  | DumpCore               -- -ddump-core
  | DumpTypes              -- -ddump-types
  | DumpKinds              -- -ddump-kinds
  | DumpStg                -- -ddump-stg
  | DumpImp                -- -ddump-imp
  | DumpRenamer            -- -ddump-rn
  | DumpToFile             -- -ddump-to-file
  deriving (Eq, Ord, Show)

-- | Flag set
type Flags = S.Set Flag

-- Operations on the Flags set

-- | Query a flag setting
isSet :: Flags -> Flag -> Bool
isSet = flip S.member

-- | Insert a flag into the Flags set
set :: Flags -> Flag -> Flags
set = flip S.insert

-- | Remove a flag from the flags set
unset :: Flags -> Flag -> Flags
unset = flip S.delete

-- | A list of associations with the string and Haskell representations
--   of all the compiler flags
flags :: [(String, Flag)]
flags = [
    ("ddump-parsed",     DumpParsed),
    ("ddump-ds",         DumpDesugar),
    ("ddump-infer",      DumpInfer),
    ("ddump-core",       DumpCore),
    ("ddump-types",      DumpTypes),
    ("ddump-kinds",      DumpKinds),
    ("ddump-stg",        DumpStg),
    ("ddump-imp",        DumpImp),
    ("ddump-rn",         DumpRenamer),
    ("ddump-to-file",    DumpToFile)
    ]

matches :: String -> (String,Flag) -> Maybe Flag
matches s (flagstr, flag)
    | ('-' : flagstr) `isPrefixOf` s = Just flag
    | otherwise = Nothing

-- | Command line switches for flag options
flagOpts :: [String]
flagOpts = fmap fst flags

-- | Lookup the flag from a command line option switch
flagFor :: String -> Maybe Flags.Flag
flagFor s = msum $ fmap (matches s) flags
