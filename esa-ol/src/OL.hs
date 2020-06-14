module OL
    ( Program(..)
    , eval
    ) where

import AST
import Parse
import Data.Char
import Data.Text as T
import Data.Map.Strict as MS
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

type Env = Map Variable Int


evalExpr :: Expr -> StateT Env (Either Text) Int
evalExpr (Int d) = pure d
evalExpr (Var v) = get >>= evalInEnv v
    where
        evalInEnv :: Text -> Env -> StateT Env (Either Text) Int
        evalInEnv v env = 
            case MS.lookup v env of
              Nothing -> lift $ Left (T.append (T.pack "used undefined var ") v)
              Just d -> lift $ Right d

evalStmt :: Stmt -> StateT Env (Either Text) ()
evalStmt (Assign var expr) = do
    val <- evalExpr expr 
    modify $ MS.insert var val

eval :: Program -> Either Text Double
eval (Program stmts) = evalStateT prog MS.empty
    where
        prog = do
            traverse evalStmt stmts
