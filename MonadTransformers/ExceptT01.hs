-- --------------------------------------------------------------------------
-- Login00.hs
-- --------------------------------------------------------------------------
--
-- See: 
--   https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md
--
-- This file contains the first part  till the exceptions
-- --------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative


-- --------------------------------------------------------------------------
-- Either - Left or Right
-- --------------------------------------------------------------------------

data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
    deriving Show


getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
     [name, domain] -> Right domain
     _              -> Left InvalidEmail

printResult' :: Either LoginError Text -> IO()
printResult' domain =
  case domain of
     Right text         -> T.putStrLn $ append "Domain: " text
     Left InvalidEmail  -> T.putStrLn "ERROR: Invalid domain"

-- >>> printResult' (getDomain "test@example.com")
-- >>> printResult' (getDomain "test@example@com")

-- use the either function
-- either (a -> c) -> (b -> c) -> Either a b -> c
-- either: The main parameter is Either a b, 
--           if it's Left a   -> then the first function is applied
--           if it's Reight b -> then the second function is applied


-- Rewrite printResult' with either, 
--    so the pattern matching is done inside either
--    Note: this is pointfree! 
--    Note: Use of const to get a function

-- | Print out the result of a call to the getDomain function
printResult1 :: Either LoginError Text -> IO()
printResult1 = T.putStrLn . either
   (const "ERROR: Invalid domain")
   (append "Domain: ")

-- ---------------------------------------------------------------------------
-- Introducing Side-Effects
-- ---------------------------------------------------------------------------

-- | Ask the user the user for his mail address and get out the domain
getToken1 :: IO (Either LoginError Text)
getToken1 = do
  T.putStrLn "Enter your mail address"
  email <- T.getLine
  return (getDomain email)

-- We have 2 users in our password database
users :: Map Text Text
users = Map.fromList[("example.com", "qwertz"), ("localhost", "password")]
       

-- A first but ugly function to read and verify mailaddress and password
userLogin1 :: IO (Either LoginError Text)
userLogin1 = do
    token <- getToken1

    case token of
      Right domain -> 
        case Map.lookup domain users of
          Just userpw -> do
            putStrLn "Enter password:"
            password <- T.getLine
            if userpw == password
              then return token
              else return $ Left WrongPassword
          Nothing -> return $ Left NoSuchUser
      left -> return left

-- ----------------------------------------------------------------------------
-- Lets Make Our Own Monad - a combination of IO and Either
-- ----------------------------------------------------------------------------
data EitherIO e a = EitherIO {
  runEitherIO :: IO (Either e a)
  }


-- Note: This is a type transformer:
--       It allows us to go from the combinations we used above
--       to our own type and back again.

-- :t EitherIO
-- EitherIO :: IO (Either e a) -> EitherIO e a  
-- wrap: from combination to own type

-- :t runEitherIO
-- runEitherIO :: EitherIO e a -> IO (Either e a) 
-- unwrap: from own type to combination

instance Functor (EitherIO e) where
  fmap f = EitherIO . fmap (fmap f) . runEitherIO
  -- unwrap -> apply to double packed value -> wrap up again

instance Applicative (EitherIO e) where
  pure = EitherIO . return . Right
  -- (<*>) :: f (a -> b) -> f a -> f b
  f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)

instance Monad (EitherIO e) where
  return = pure
  x >>= f = EitherIO $ runEitherIO x >>= either (return . Left) (runEitherIO .f)

-- -----------------------------------------------------------------------------
-- Using EitherIO
-- -----------------------------------------------------------------------------
getToken2 :: EitherIO LoginError Text
getToken2 = do
  EitherIO $ fmap Right (T.putStrLn "Enter e-mail address: ")
  input <- EitherIO $ fmap Right T.getLine
  EitherIO $ return (getDomain input)

-- | Lift an Either value into an EitherIO value
liftEither :: Either e a -> EitherIO e a
liftEither = EitherIO . return

-- | Lift an IO value into an EitherIO value
liftIO :: IO a -> EitherIO e a
liftIO = EitherIO . fmap Right

getToken3 :: EitherIO LoginError Text
getToken3 = do
   liftIO $ T.putStrLn "Enter e-mail address: "
   input <- liftIO T.getLine
   liftEither $ getDomain input


-- | New version of userlogin and printResult2 
userLogin3 :: EitherIO LoginError Text
userLogin3 = do
  token    <- getToken3
  userpw   <- maybe (liftEither (Left NoSuchUser)) -- lifts an Either value
               return 					          -- return a :: EitherIO e a
               ((Map.lookup token users))    -- 3. argument of maybe : A Maybe
  password <- liftIO (T.putStrLn "Enter your password" >> T.getLine)

  if userpw == password
    then return token
    else liftEither $ Left WrongPassword


printResult2 :: EitherIO LoginError Text -> IO()
printResult2 eio = do
   r <- runEitherIO eio
   (T.putStrLn . either
     (const "ERROR: Invalid domain")
     (append "Domain: ") ) r

-- ----------------------------------------------------------------------------
-- Throwing errors
-- ----------------------------------------------------------------------------
throwE :: e -> EitherIO e a
throwE x = liftEither (Left x)

userLogin :: EitherIO LoginError Text
userLogin = do
  token    <- getToken3
  userpw   <- maybe (throwE NoSuchUser) -- lifts a (Left) Either value
               return 					          -- return a :: EitherIO e a
               ((Map.lookup token users))    -- 3. argument of maybe : A Maybe
  password <- liftIO (T.putStrLn "Enter your password" >> T.getLine)

  if userpw == password
    then return token
    else liftEither $ Left WrongPassword








  




