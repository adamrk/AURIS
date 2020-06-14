module OL
    ( Program(..)
    , parseProgram
    , eval
    ) where

import Data.Char
import Data.Text as T
import Data.Attoparsec.Text as AT
import Data.Map.Strict as MS
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

data Expr = Lit Double
    | Var Var
    deriving (Eq, Show)

type Var = Text 

data Stmt = Assign Var Expr
    deriving (Eq, Show)

data Program = Program [Stmt] Expr
    deriving (Eq, Show)

type Env = Map Var Double

parseExpr :: Parser Expr
parseExpr = do
    expr <- AT.takeWhile1 (\c -> c /= '\n' && c /= ';')
    if T.beginsWith "VAR" then return (Var expr) else return $ Lit $ read $ T.unpack expr

parseAssign :: Parser Stmt
parseAssign = do
    AT.takeWhile isSpace
    AT.string $ T.pack "VAR"
    var <- AT.takeWhile1 (not . isSpace)
    AT.takeWhile isSpace
    AT.string $ T.pack ":="
    AT.takeWhile isSpace
    expr <- parseExpr
    AT.string $ T.pack ";" 
    return $ Assign var expr

parseProgram :: Parser Program
parseProgram = do
    stmts <- sepBy' parseAssign $ char '\n'
    AT.takeWhile isSpace
    ret <- parseExpr
    AT.string $ T.pack ";"
    return $ Program stmts ret

evalExpr :: Expr -> StateT Env (Either Text) Double
evalExpr (Lit d) = pure d
evalExpr (Var v) = get >>= evalInEnv v
    where
        evalInEnv :: Text -> Env -> StateT Env (Either Text) Double
        evalInEnv v env = 
            case MS.lookup v env of
              Nothing -> lift $ Left (T.append (T.pack "used undefined var ") v)
              Just d -> lift $ Right d

evalStmt :: Stmt -> StateT Env (Either Text) ()
evalStmt (Assign var expr) = do
    val <- evalExpr expr 
    modify $ MS.insert var val

eval :: Program -> Either Text Double
eval (Program stmts ret) = evalStateT prog MS.empty
    where
        prog = do
            traverse evalStmt stmts
            evalExpr ret