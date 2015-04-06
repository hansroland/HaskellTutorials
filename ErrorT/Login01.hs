-- --------------------------------------------------------------------------
-- Login01.hs
-- --------------------------------------------------------------------------
--
-- See: 
--   https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md
--
-- This file contains the second part with the ExceptIO instead of EitherIO
-- --------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative

-- --------------------------------------------------------------------------
-- ExceptIO Datatype: Definition and Properties
-- --------------------------------------------------------------------------
data ExceptIO e a = ExceptIO {
  runExceptIO :: IO (Either e a)
  }

-- :t ExceptIO
-- ExceptIO :: IO (Either e a) -> ExceptIO e a  
-- wrap: from combination to own type

-- :t runExceptIO
-- runExceptIO :: ExceptIO e a -> IO (Either e a) 
-- unwrap: from own type to combination

instance Functor (ExceptIO e) where
  fmap f = ExceptIO . fmap (fmap f) . runExceptIO
  -- unwrap -> apply to double packed value -> wrap up again

instance Applicative (ExceptIO e) where
  pure = ExceptIO . return . Right
  -- (<*>) :: f (a -> b) -> f a -> f b
  f <*> x = ExceptIO $ liftA2 (<*>) (runExceptIO f) (runExceptIO x)

instance Monad (ExceptIO e) where
  return = pure
  x >>= f = ExceptIO $ runExceptIO x >>= either (return . Left) (runExceptIO .f)


printExcept :: (Show a, Show e) => ExceptIO e a -> IO()
printExcept x = do
   runExceptIO x >>= print


-- Lifting

-- | Lift an Either value into an EitherIO value
liftEither :: Either e a -> ExceptIO e a
liftEither = ExceptIO . return

-- | Lift an IO value into an EitherIO value
liftIO :: IO a -> ExceptIO e a
liftIO = ExceptIO . fmap Right

throwE :: e -> ExceptIO e a
throwE x = liftEither (Left x)

catchE :: ExceptIO e a -> (e -> ExceptIO e a) -> ExceptIO e a
catchE throwing handler = 
    ExceptIO $ do 
      result <- runExceptIO throwing
      case result of
        Left failure -> runExceptIO (handler failure)
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


getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
     [name, domain] -> Right domain
     _              -> Left InvalidEmail

getToken :: ExceptIO LoginError Text
getToken = do
   liftIO $ T.putStrLn "Enter e-mail address: "
   input <- liftIO T.getLine
   liftEither $ getDomain input

userLogin :: ExceptIO LoginError Text
userLogin = do
  token    <- getToken
  userpw   <- maybe (throwE NoSuchUser) -- lifts a (Left) Either value
               return 					          -- return a :: EitherIO e a
               ((Map.lookup token users))    -- 3. argument of maybe : A Maybe
  password <- liftIO (T.putStrLn "Enter your password" >> T.getLine)

  if userpw == password
    then return token
    else throwE WrongPassword

-- usage:
-- >>> runExceptIO userLogin
-- >>>>  name@example.com
-- >>>>  qwertz

wrongPasswordHandler :: LoginError -> ExceptIO LoginError Text
wrongPasswordHandler WrongPassword = do
    liftIO (T.putStrLn "Wrong password, one more chance.")
    userLogin
wrongPasswordHandler err = throwE err

printError :: LoginError -> ExceptIO LoginError a
printError err = do
    liftIO . T.putStrLn $ case err of
      WrongPassword -> "Wrong password. No more chances."
      NoSuchUser    -> "No user with this email address."
      InvalidEmail  -> "Invalid email address entered."
    throwE err

loginDialogue :: ExceptIO LoginError ()
loginDialogue = do
    let retry = userLogin `catchE` wrongPasswordHandler
    token      <- retry   `catchE` printError
    liftIO $ T.putStrLn (append "logged in with token: " token)

main :: IO()
main = do
     runExceptIO loginDialogue
     return ()

