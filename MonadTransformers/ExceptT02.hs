-- --------------------------------------------------------------------------
-- ExceptT02.hs
-- --------------------------------------------------------------------------
--
-- See: 
--   https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md
--
-- This file contains the third part with the ExceptT instead of ExceptIO
-- --------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative

-- ----------------------------------------------------------------------------
-- ExceptT Datatype: Definition and Properties
-- ----------------------------------------------------------------------------
data ExceptT e m a = ExceptT {
  runExceptT :: m (Either e a)
  }

-- :t ExceptT
-- ExceptT :: m (Either e a) -> ExceptT e m a  
-- wrap: from combination to own type

-- :t runExceptT
-- runExceptT :: ExceptT e m a -> m (Either e a) 
-- unwrap: from own type to combination

instance Functor m => Functor (ExceptT e m) where
  fmap f = ExceptT . fmap (fmap f) . runExceptT
  -- unwrap -> apply to double packed value -> wrap up again

instance Applicative m => Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right
  -- (<*>) :: f (a -> b) -> f a -> f b
  f <*> x = ExceptT $ liftA2 (<*>) (runExceptT f) (runExceptT x)

instance Monad m => Monad (ExceptT e m) where
  return = ExceptT . return . Right
  x >>= f = ExceptT $ runExceptT x >>= either (return . Left) (runExceptT .f)

{-
-- This we cannot gneralize from IO to m
printExcept :: (Show a, Show e) => ExceptIO e a -> IO()
printExcept x = do
   runExceptIO x >>= print
-}


-- Lifting

-- | Lift an Either value into an EitherIO value
liftEither :: Monad m => Either e a -> ExceptT e m a
liftEither = ExceptT . return

-- | Lift an monad value into an EitherIO value
lift :: Functor m => m a -> ExceptT e m a
lift = ExceptT . fmap Right

throwE :: Monad m => e -> ExceptT e m a
throwE x = liftEither (Left x)

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT e m a) -> ExceptT e m a
catchE throwing handler = 
    ExceptT $ do 
      result <- runExceptT throwing
      case result of
        Left failure -> runExceptT (handler failure)
        Right success-> return $ Right success

-- ----------------------------------------------------------------------------
-- Application
-- ----------------------------------------------------------------------------
-- | A little database with 2 users in our password database
users :: Map Text Text
users = Map.fromList[("example.com", "qwertz"), ("localhost", "password")]

-- | Data type with possible failure states
data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
    deriving Show

-- | Get the domain out of the mail name (simple version)
getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
     [name, domain] -> Right domain
     _              -> Left InvalidEmail

getToken :: ExceptT LoginError IO Text
getToken = do
   lift $ T.putStrLn "Enter e-mail address: "
   input <- lift T.getLine
   liftEither $ getDomain input

userLogin :: ExceptT LoginError IO Text
userLogin = do
  token    <- getToken
  userpw   <- maybe (throwE NoSuchUser) -- lifts a (Left) Either value
               return 					          -- return a :: EitherIO e a
               ((Map.lookup token users))    -- 3. argument of maybe : A Maybe
  password <- lift (T.putStrLn "Enter your password" >> T.getLine)

  if userpw == password
    then return token
    else throwE WrongPassword

-- usage:
-- >>> ExceptT userLogin
-- >>>>  name@example.com
-- >>>>  qwertz

wrongPasswordHandler :: LoginError -> ExceptT LoginError IO Text
wrongPasswordHandler WrongPassword = do
    lift (T.putStrLn "Wrong password, one more chance.")
    userLogin
wrongPasswordHandler err = throwE err

printError :: LoginError -> ExceptT LoginError IO a
printError err = do
    lift . T.putStrLn $ case err of
      WrongPassword -> "Wrong password. No more chances."
      NoSuchUser    -> "No user with this email address."
      InvalidEmail  -> "Invalid email address entered."
    throwE err

loginDialogue :: ExceptT LoginError IO ()
loginDialogue = do
    let retry = userLogin `catchE` wrongPasswordHandler
    token      <- retry   `catchE` printError
    lift $ T.putStrLn (append "logged in with token: " token)

main :: IO()
main = do
     runExceptT loginDialogue
     return ()

