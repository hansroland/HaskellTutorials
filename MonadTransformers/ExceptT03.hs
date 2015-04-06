-- --------------------------------------------------------------------------
-- Except03.hs
-- --------------------------------------------------------------------------
--
-- See: 
--   https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md
--
-- This is the fourth part using the library Control.Monad.Trans.Except
--
-- If loading the module gives an ambiguous module name
-- use : :set -hide-package xxx
-- where xx = <a-package-name-different than transformers>
-- --------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative
import Control.Monad.Trans.Except


-- Lifting: the 2 lifting functions are obviously missing from the library

-- | Lift an monad value into an EitherIO value
lift :: Functor m => m a -> ExceptT e m a
lift = ExceptT . fmap Right

-- | Lift an Either value into an EitherIO value
liftEither :: Monad m => Either e a -> ExceptT e m a
liftEither = ExceptT . return


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

