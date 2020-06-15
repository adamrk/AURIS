{-# LANGUAGE TemplateHaskell #-}

module Eval
    ()
    where

import Control.Lens                      ( makeLenses, over, set, view, (^.), (%~))
import Control.Monad.Trans.Class         ( lift )
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Data.Map.Strict as Map
import Data.TM.Parameter
import Data.TM.Value
import General.Time
import AST

data Env = Env {
      _eLastReturn :: !TMValueSimple
    , _eLocalVars :: !(Map.Map Variable TMValueSimple)
    , _eGlobalVars :: !(Map.Map Variable TMValueSimple)
    , _eSystemTime :: !SunTime
} deriving (Show)
makeLenses ''Env

data FinishState 
    = Returned TMValueSimple
    | Errored String
    deriving (Show)


evalProgram :: Program -> ExceptT FinishState (State Env) TMValueSimple
evalProgram (Program stmts) = do
    traverse evalStmt stmts
    lift $ gets (view eLastReturn)

evalStmt :: Stmt -> ExceptT FinishState (State Env) ()
evalStmt (LAssign var expr) = do
    val <- evalExpr expr
    lift $ modify' (over eLocalVars $ Map.insert var val) 
evalStmt (GAssign var expr) = do
    val <- evalExpr expr
    lift $ modify' (over eGlobalVars $ Map.insert var val) 
evalStmt (If expr stmts) = do
    val <- evalExpr expr
    if isTruthy val
        then mapM_ evalStmt stmts
        else return ()
evalStmt (IfElse expr ifStmts elseStmts) = do
    val <- evalExpr expr
    if isTruthy val
        then mapM_ evalStmt ifStmts
        else mapM_ evalStmt elseStmts
evalStmt (ReturnExpr expr) = do
    val <- evalExpr expr
    lift $ modify' (set eLastReturn $ val)
evalStmt (ReturnStmt expr) = do
    val <- evalExpr expr
    throwE $ Returned val

evalExpr :: Expr -> ExceptT FinishState (State Env) TMValueSimple
evalExpr (Param param view) = _
evalExpr (StaticParam param view) = _
evalExpr (Var var) = do
    env <- lift get
    case Map.lookup var (env^.eLocalVars) of
        Just val -> return val
        Nothing -> throwE $ Errored $ "Undefined local var " ++ var
evalExpr (GVar var) = do
    env <- lift get
    case Map.lookup var (env^.eGlobalVars) of
        Just val -> return val
        Nothing -> throwE $ Errored $ "Undefined global var " ++ var
evalExpr (Int n) = return $ TMValInt n
evalExpr SystemTime = do
    env <- lift get
    return $ TMValTime $ env^.eSystemTime
evalExpr (Negate e) = do
    inner <- evalExpr e
    case inner of
        TMValInt n -> return $ TMValInt (- n)
        TMValDouble n -> return $ TMValDouble (- n)
        _ -> throwE $ Errored $ "Invalid negation argument " ++ show inner
evalExpr _ = _

isTruthy :: TMValueSimple -> Bool
isTruthy = _