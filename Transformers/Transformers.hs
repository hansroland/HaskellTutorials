-- ----------------------------------------------------------------------------
-- Transformers.hs
-- ----------------------------------------------------------------------------
--
-- See Martin Grabmüller: Monad Transformers Step by Step
-- 
-- An Introduction how to use Monad Transformers
--
--
-- ----------------------------------------------------------------------------

module Transformers where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map


-- Data types for an interpreter for a simple language

type Name = String

-- | Expressions in our language (aka Programs)
data Exp = Lit Integer        -- ^ Integer literals
         | Var Name           -- ^ Variables
         | Plus Exp Exp       -- ^ Addition ooperation
         | Abs Name Exp       -- ^ λ expressions (abstractions)
         | App Exp Exp        -- ^ Function applications
         deriving (Show)

-- | Values (aka Results of our program
data Value = IntVal Integer   -- ^ Integer values
           | FunVal Env Name Exp  -- ^ Functions (closures)
           deriving (Show)

-- | Environments in which a function is evaluated (= variable dictionary)
type Env = Map.Map Name Value

---------------------------------------------------------------------
-- The Non-Monadic version of the interpreter
-- ------------------------------------------------------------------

-- | eval0 : A non monadic interpreter for our language
--   Note: It has some issues: 
--     Function fromJust may crash...
--     Pattern matching in the App constructor is incomplete
eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = 
    let IntVal i1 = eval0 env e1
        IntVal i2 = eval0 env e2
    in IntVal(i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) =
    let val1 = eval0 env e1
        val2 = eval0 env e2
    in case val1 of
        FunVal env' n body -> eval0 (Map.insert n val2 env') body

-- | A simple example to test our different evaluators
exampleExp :: Exp
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

-- | A bad example: x is not defined
badExp :: Exp
badExp = (Plus (Lit 1) (Abs "x" (Var "x")))

-- | Undefined variable
undefExp :: Exp
undefExp = (Var "x")

-- >>> eval0 Map.empty exampleExp
-- IntVal 18

--------------------------------------------------------------------
-- The Most-Simple-Monadic version of the interpreter
-- -----------------------------------------------------------------

-- | Use the Identity Monad to evaluate a program
type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 env = runIdentity env    -- (env could be removed)

-- | eval1 : A simpel monadic evaluator for our little language
eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = maybe (fail ("undef variable: " ++ n)) 
       return $ Map.lookup n env
eval1 env (Plus e1 e2) = do
    IntVal i1 <- eval1 env e1
    IntVal i2 <- eval1 env e2
    return $ IntVal(i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do
    val1 <- eval1 env e1
    val2 <- eval1 env e2
    case val1 of
        FunVal env' n body -> eval1 (Map.insert n val2 env') body

-- >>> runEval1 (eval1 Map.empty exampleExp)
-- IntVal 18

-- ----------------------------------------------------------------
-- Adding Error Handling: Use the ErrorT Transformer
-- ----------------------------------------------------------------

-- | Extend the simple type Eval1 with the ErrorT Transformer
type Eval2 a = ErrorT String Identity a

-- | The run function for the evaluator with error handling
-- Note: Compared to eval1 we replaced 
--       eval1 -> eval2a
--       Eval1 -> Eval2
--    so in the end we changed just the type !! 
runEval2 :: Eval2 a -> Either String a
runEval2 env = runIdentity (runErrorT env)

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = maybe (fail ("undef variable: " ++ n)) 
       return $ Map.lookup n env
eval2a env (Plus e1 e2) = do
    IntVal i1 <- eval2a env e1
    IntVal i2 <- eval2a env e2
    return $ IntVal(i1 + i2)
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2) = do
    val1 <- eval2a env e1
    val2 <- eval2a env e2
    case val1 of
        FunVal env' n body -> eval2a (Map.insert n val2 env') body


-- >>> runEval2 (eval2a Map.empty exampleExp)
-- Right (IntVal 18)

-- >>> runEval2 (eval2a Map.empty badExp)
-- Gives a failure

-- | Modified eval2a to use the error reporting of the errorT Transformer
eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i) = return $ IntVal i
eval2b env (Var n) = maybe (fail ("undef variable: " ++ n)) 
       return $ Map.lookup n env
eval2b env (Plus e1 e2) = do    -- Improved to check the evaluation results 
    e1' <- eval2b env e1
    e2' <- eval2b env e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal(i1 + i2)
        _ -> throwError "type error"
eval2b env (Abs n e) = return $ FunVal env n e
eval2b env (App e1 e2) = do
    val1 <- eval2b env e1
    val2 <- eval2b env e2
    case val1 of
        FunVal env' n body -> eval2b (Map.insert n val2 env') body
        _                  -> throwError "type Error"    -- Added !!

-- >>> runEval2 (eval2b Map.empty badExp)
-- Left "type error"

-- >>> runEval2 (eval2b Map.empty exampleExp)
-- Right (IntVal 18)


-- | The final version for  evaluation with error messages
eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
    Nothing  -> throwError ("undef variable: " ++ n)
    Just val -> return val
eval2 env (Plus e1 e2) = do    -- Improved to check the evaluation results
    e1' <- eval2 env e1
    e2' <- eval2 env e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal(i1 + i2)
        _ -> throwError "type error"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do
    val1 <- eval2 env e1
    val2 <- eval2 env e2
    case val1 of
        FunVal env' n body -> eval2 (Map.insert n val2 env') body
        _                  -> throwError "type Error"    -- Added !!

-- >>> runEval2 (eval2 Map.empty badExp)
-- Left "type error"

-- >>> runEval2 (eval2 Map.empty exampleExp)
-- Right (IntVal 18)

-- >> runEval2 (eval2 Map.empty undefExp)
-- Left "undef variable: x"


