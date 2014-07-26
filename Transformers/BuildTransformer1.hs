-- Building Monad Transformers Step by Step
--
-- See: http://blog.jakubarnold.cz/2014/07/22/building-monad-transformers-part-1.html
-- 

import Control.Applicative

-- ---------------------------------------------------------------------------
-- The base problem: Searching a database
-- ---------------------------------------------------------------------------

-- | A very simple data type
data User = User deriving Show

-- | A simulator for a database query function
findById :: Int -> IO (Maybe User)
findById 1 = return $ Just User
findById _ = return Nothing

-- | A first (UGLY) version of a function to find 2 useres
findUsers :: Int -> Int -> IO (Maybe (User,User))
findUsers x y = do
    muser1 <- findById x
    case muser1 of
        Nothing -> return Nothing
        Just user1 -> do
            muser2 <- findById y
            case muser2 of
                Nothing -> return Nothing
                Just user2 -> do
                    return $ Just (user1, user2)
-- >>> findUsers 1 1
-- Just (User,User)

-- >>> findUsers 1 0
-- Nothing

-- ----------------------------------------------------------------------------
-- A new data type MaybeIO
 ----------------------------------------------------------------------------
data MaybeIO a = MaybeIO {runMaybeIO :: IO (Maybe a)}

-- | To make it clear which fmap we use
mbFmap :: (a -> b) -> Maybe a -> Maybe b
mbFmap = fmap

ioFmap :: (a -> b) -> IO a -> IO b
ioFmap = fmap

-- | Make MaybeIO a Functor
-- Note: 
-- The type for fmap must be: fmap :: (a -> b) -> MaybeIO a -> MaybeIO b
instance Functor MaybeIO where
   fmap f (MaybeIO m) = MaybeIO $ (fmap . fmap) f m

-- To be exact, this means
--   fmap f (MaybeIO m) = MaybeIO $ (ioFmap . mbFmap) f m


-- Definiton of class Applicative
-- class Applicative where
--     pure  :: a -> m a
--     (<$>) :: m(a -> b) -> m a -> m b

-- | Make MaybeIO an Applicative
--  pure: wrap it twice: first into a Maybe, then into an IO
--  <$> is a little bit more tricky: 
--       We have to lift a function with 2 arguments 
instance Applicative MaybeIO where
   pure = MaybeIO . pure . pure
   MaybeIO f <*> MaybeIO m = MaybeIO $ liftA2 (<*>) f m  


-- Definiton of class Monad
-- class Monad where
--    return :: a -> m a
--    (>==)  :: m a -> (a -> m b) -> m b

-- | Make MaybeIO a Monad
instance Monad MaybeIO where
    return = pure
    MaybeIO m >>= f = MaybeIO $ m >>= \x -> case x of
       Nothing -> return $ Nothing
       Just val -> runMaybeIO $ f val

-- | Improve the findUsers Example
transformerFindUsers :: Int -> Int -> MaybeIO (User, User)
transformerFindUsers x y = do
    user1 <- MaybeIO $ findById x
    user2 <- MaybeIO $ findById y
    return (user1, user2)

-- | Make smartFindUsers the same type as findUsers
-- Unwrap it with the runMaybeIO after the do
smartFindUsers :: Int -> Int -> IO (Maybe (User, User))
smartFindUsers x y = runMaybeIO $ do
    user1 <- MaybeIO $ findById x
    user2 <- MaybeIO $ findById y
    return (user1, user2)

-- >>> smartFindUsers 1 1
-- Just (User,User)

-- >>> smartFindUsers 1 0
-- Nothing

-- Generalizing MaybeIO to MaybeT
-- Replace IO with any Monad m 

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

-- | Make MaybeT a Monad
instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . Just
    MaybeT m >>= f = MaybeT $ 
       do 
         val <- m
         case val of
           Nothing -> return Nothing
           Just x  -> runMaybeT $ f x

-- | Our first example with this transformer
wrappedFindUsers :: Int -> Int -> MaybeT IO (Maybe (User, User))
wrappedFindUsers x y = do
    user1 <- MaybeT $ findById x
    user2 <- MaybeT $ findById y
    return $ Just (user1, user2)


