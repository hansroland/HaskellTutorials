-- ----------------------------------------------------------------------------
-- Compiler.hs
-- ----------------------------------------------------------------------------

module Compiler (
    -- Code paths
    modl,
    expr ,

    -- Module driver
    modls,
    ) where

-- | modl path - Compiler entry to compile whole modules
--   modl models the compiler pipeline
modl = undefined
{-
modl :: FilePath -> L.Text -> CompilerM ()
modl fname = parseP fname
  >=> dataP
  >=> groupP
  >=> renameP
  >=> desugarP
  >=> inferP
  >=> evalP
-}


-- | expr - Compiler entry for interactive evaluation
--   expects an expression and joins it into the interactive environnement.
expr = undefined

modls = undefined
