-- ---------------------------------------------------------------------------
-- Directories.hs
-- ---------------------------------------------------------------------------

module Directories
   where

import System.Directory


defaultDir :: FilePath
defaultDir = "/home/roland/Bilder"

-- | Return all the subdirectories from a given directory
subdirs :: FilePath -> IO [String]
subdirs dir =
    getDirectoryContents $ defaultDir ++ dir
