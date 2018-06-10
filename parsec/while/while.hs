-- This is almost entirely copied from: https://wiki.haskell.org/Parsing_a_simple_imperative_language

module ParseWhile where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- numeric
--   = number
--   | - numeric
--   | numeric opa numeric
-- logical
--   = true
--   | false
--   | not logical
--   | logical opb logical
--   | numeric opr numeric
-- opa = + | - | * | /
-- opb = and | or
-- opr = > | <

-- statement
--   = identifier := numeric
--   | skip
--   | statement; statement
--   | ( statement )
--   | if b then statement else statement | while b do statement

data BExpr
  = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr
   deriving (Show)


data AExpr
  = Var String
  | IntConst Integer
  | Negate AExpr 
  | ABinary ABinOp AExpr AExpr 
  deriving(Show)

data BBinOp = And | Or deriving (Show)
data RBinOp = Greater | Less deriving (Show)
data ABinOp = Plus | Minus | Multiply | Divide deriving (Show)

data Statement
  = Assign String AExpr
  | Skip
  | Block [Statement]
  | Paren Statement
  | Conditional BExpr Statement Statement
  | While BExpr Statement

languageDef =
  emptyDef {
      Token.commentStart    = "/*"
    , Token.commentEnd      = "*/"
    , Token.commentLine     = "//"
    , Token.identStart      = letter
    , Token.identLetter     = alphaNum
    , Token.reservedNames   = ["if", "then", "else", "while", "do", "skip", "true", "false"]
    , Token.reservedOpNames = ["+", "-", "*", "/", ":=", "<", ">", "not", "and", "or"]
  }

lexer = Token.makeTokenParser languageDef

-- whitespace between tokens is automatically handled
identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer

-- This is the top-level parser
whileParser :: Parser Statement 
-- Have to remove initial whitespace, internal whitespace is handled by lexer 
whileParser = whiteSpace >> statement

statement :: Parser Statement
statement = parens statement <|> sequenceOfStatements

sequenceOfStatements =
  do list <- (sepBy1 statement' semi)
     return $ case list of
       [s] -> s
       xs  -> Block list

statement' :: Parser Statement
statement'
  =   parens statement
  <|> ifStatement
  <|> whileStatement
  <|> skipStatement
  <|> assignStatement


ifStatement :: Parser Statement
ifStatement = do
  reserved "if"
  cond <- bExpression
  reserved "then"
  body <- statement
  reserved "else"
  body <- statement
  return $ Conditional cond body body
  

whileStatement :: Parser Statement
whileStatement = do
  reserved "while"
  cond <- bExpression
  reserved "do"
  body <- statement
  return $ While cond body

skipStatement :: Parser Statement
skipStatement = reserved "skip" >> return Skip

assignStatement :: Parser Statement
assignStatement = do
  var <- identifier 
  reservedOp ":="
  expr <- expression
  return $ Assign var expr


aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm
 
bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
             , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft,
                Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
              ]

bOperators = [ [Prefix (reservedOp "not" >> return (Not             ))          ]
             , [Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft,
                Infix  (reservedOp "or"  >> return (BBinary Or      )) AssocLeft]
             ]

aTerm =  parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer

bTerm =  parens bExpression
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))
     <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RBinary op a1 a2

relation = (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)
