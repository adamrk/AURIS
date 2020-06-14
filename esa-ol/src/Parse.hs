{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( pProgram
    ) where

import AST
import Data.Text as T
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text

-- see https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec.html#g:3 on how to run the parser
pProgram :: Parser Program
pProgram = Program <$> pStmtSeq


-- Helpers

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "#")
  (L.skipBlockComment "/*" "*/") -- TODO: since we don't have block comments, guess we should put mempty here? but that makes the parser never terminate on any input

-- | @lexeme p@ parses @p@ and consumes any whitespace that follows
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | @symbol "x"@ parses the string @x@ and consumes any whitespace that follows
symbol :: Text -> Parser Text
symbol = L.symbol sc

semi :: Parser ()
semi = void $ symbol ";"

-- | parses the reserved word @w@
rword :: Text -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

-- TODO: use this in all the right places, see https://github.com/mrkkrp/megaparsec-site/blob/master/tutorials/parsing-simple-imperative-language.md
reservedWords :: [Text]
reservedWords = ["if", "then", "else", "endif", "return", "TRUE", "FALSE"
    , "sin", "cos", "tan", "cotan", "arcsin", "arccos", "arctan", "arccotan"
    , "system_time", "system_date"
    ]


-- Statements

pStmtSeq :: Parser [Stmt]
pStmtSeq = sepEndBy1 pStmt semi

pStmt :: Parser Stmt
pStmt = choice
  [ pIf
  , pReturnExpr
  ]

pReturnExpr :: Parser Stmt
pReturnExpr = ReturnExpr <$> pExpr

pIf :: Parser Stmt
pIf = do
  rword "if"
  cond  <- pExpr
  rword "then"
  stmt <- pStmtSeq
  rword "endif"
  return $ If cond stmt



-- Expressions

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , pVar
  , pGVar
  , pSystemTime
  , pSystemDate
  , pStaticParam
  , pParam -- pParam swallows quite a lot, so needs to come late in the choice
  , pInteger
  ]

pStaticParam :: Parser Expr
pStaticParam = char '$' *> buildParamParser StaticParam

pParam :: Parser Expr
pParam = buildParamParser Param

buildParamParser :: (Text -> ParamView -> Expr) -> Parser Expr
buildParamParser paramType = paramType <$> param <*> suffix
  where
      param = pack <$>
        ((:) <$> letterChar <*> many alphaNumChar <?> "monitoring parameter")
      suffix = lexeme $
          (char '.' *> view) <|> pure None
      view = choice
          [ Raw <$ symbol "raw"
          , Eng <$ symbol "eng"
          , Time <$ symbol "time"
          ]

pVar :: Parser Expr
pVar = Var <$> lexeme
    ((string "VAR") *> many alphaNumChar <?> "local variable")

pGVar :: Parser Expr
pGVar = GVar <$> lexeme
    ((string "GVAR") *> many alphaNumChar <?> "global variable")

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

pSystemTime :: Parser Expr
pSystemTime = SystemTime <$ symbol "system_time"

pSystemDate :: Parser Expr
pSystemDate = SystemDate <$ symbol "system_date"


-- following inspired heavily by https://markkarpov.com/tutorial/megaparsec.html#parsing-expressions

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" Negate
    , prefix "+" id
    ]
  , [ binary "*" Mult
    , binary "/" Div
    ]
  , [ binary "+" Add
    , binary "-" Sub
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)
