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

data Program = Program [Stmt]
    deriving (Eq, Show)

data Stmt = Assign Variable Expr
    | If Expr [Stmt]
    | IfElse Expr [Stmt] [Stmt]
    -- | e.g. `5;` The last calculated value prior to exiting the OL script is used as the source value of the associated synthetic parameter.
    | ReturnExpr
    -- | e.g. `return(5);`
    | ReturnStmt Expr
    deriving (Eq, Show)

type Variable = Text

data Expr = Lit Text -- TODO: either int, bool, double, string, time, etc.
    -- | Monitoring parameter
    | Param Text ParamView
    -- | Static reference to a monitoring parameter, e.g. `$_T1`
    | StaticParam Text ParamView
    -- | Local variable, e.g. `VAR_T1` or `VAR1`
    | Var Variable
    -- | Global variable, e.g. `GVAR_T1`
    | GVar Variable
    | Op Operator
    -- current time
    | SystemTime
    | SystemDate
    deriving (Eq, Show)

data ParamView = Raw | Eng | Time | None

data Operator = Add Expr Expr -- +
    | Sub Expr Expr -- -
    | Mult Expr Expr -- *
    | Div Expr Expr -- /
    | Power Expr Expr -- **
    | Modulus Expr Expr -- %
    | Remainder Expr Expr -- %%
    -- Relational
    | Lt Expr Expr -- >
    | Gt Expr Expr -- <
    | Lte Expr Expr -- <=
    | Gte Expr Expr -- >=
    | Eq Expr Expr -- ==
    | Neq Expr Expr -- <>
    -- Bitwise
    | ShiftRight Expr Expr -- >>
    | ShiftLeft Expr Expr -- <<
    | And Expr Expr -- and
    | Or Expr Expr -- or
    | Not Expr -- not
    | Nand Expr Expr -- nand
    | Nor Expr Expr -- nor
    | Xor Expr Expr -- xor
    -- Logical: TODO
    | Land Expr Expr -- land
    | Lor Expr Expr -- lor
    | Lnot Expr -- lnot
    -- Trigonometric
    | Sin Expr
    | ArcSin Expr
    | Cos Expr
    | ArcCos Expr
    | Tan Expr
    | ArcTan Expr
    | CoTan Expr
    | ArcCoTan Expr




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
