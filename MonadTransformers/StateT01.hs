-- ---------------------------------------------------------------------------
-- StateT01.hs
-- ---------------------------------------------------------------------------
--
-- Get unique numbers from a list.
--
-- See: https://wiki.haskell.org/Simple_StateT_use
--
-- If loading the module gives an ambiguous module name
-- use : :set -hide-package xxx
-- where xx = <a-package-name-different than mtl>
--
-- ---------------------------------------------------------------------------

import Control.Monad.State

main :: IO()
main = runStateT code [1..] >> return ()

-- | Take out and print unique sequence numbers
code :: StateT [Integer] IO ()
code = do
   x <- pop
   io $ print x
   y <- pop
   io $ print y
   return ()

-- | Pop of the next unique number from a list
pop :: StateT [Integer] IO Integer
pop = do
  (x:xs) <- get
  put xs
  return x

-- | A little helper with a simpler type, to show what is going on here
io :: IO a -> StateT [Integer] IO a
io = liftIO

-- Note:
-- get :: m s       -- Return the state from the internals of the monad
-- put :: s -> m()  -- Replace the state inside the monad
-- liftIO :: MonadIO m => IO a -> m a
