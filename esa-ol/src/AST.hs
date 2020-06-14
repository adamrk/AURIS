{-# LANGUAGE OverloadedStrings #-}

module AST
    ( Program(..)
    , Stmt(..)
    , Expr(..)
    , ParamView(..)
    , Variable(..)
    ) where

import Data.Char
import Data.Text as T

data Program = Program [Stmt]
    deriving (Eq, Show)

type Variable = String

data Stmt
    = Assign Variable Expr
    | If Expr [Stmt]
    | IfElse Expr [Stmt] [Stmt]
    -- | e.g. @5;@ The last calculated value prior to exiting the OL script is used as the source value of the associated synthetic parameter.
    | ReturnExpr Expr
    -- | e.g. @return(5);@
    | ReturnStmt Expr
    deriving (Eq, Show)

data ParamView = Raw | Eng | Time | None
    deriving (Eq, Show)

data Expr
    -- | Monitoring parameter
    = Param Text ParamView
    -- | Static reference to a monitoring parameter, e.g. @$_T1@
    | StaticParam Text ParamView
    -- | Local variable, e.g. @VAR_T1@ or @VAR1@
    | Var Variable
    -- | Global variable, e.g. @GVAR_T1@
    | GVar Variable
    -- | TODO: either int, bool, double, string, time, etc.
    | Int Int
    -- current time
    | SystemTime
    | SystemDate
    -- Operators: numeric
    | Negate Expr -- assuming this exists, e.g. @-(1 + 2)@
    | Add Expr Expr -- @+@
    | Sub Expr Expr -- @-@
    | Mult Expr Expr -- @*@
    | Div Expr Expr -- @/@
    | Power Expr Expr -- @**@
    | Modulus Expr Expr -- @%@
    | Remainder Expr Expr -- @%%@
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
    deriving (Eq, Show)
