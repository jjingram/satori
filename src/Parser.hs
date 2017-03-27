module Parser where

import Text.Parsec
import Text.Parsec.Char (char)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Token

import qualified Data.Map as Map

import qualified Lexer as L
import Syntax

integer :: Parser Expression
integer = do
  n <- L.integer
  return $ Quote $ Atom $ Integer n

symbol :: Parser Atom
symbol = do
  s <- L.identifier
  return $ Symbol s

atom :: Parser Sexp
atom = fmap Atom (fmap Integer L.integer <|> symbol)

pair :: Parser Sexp
pair =
  L.parens $ do
    car <- many1 sexp
    L.reserved "."
    cdr <- sexp
    return $ foldr Cons cdr car

list :: Parser Sexp
list =
  L.parens $ do
    lst <- many sexp
    return $ foldr Cons (Atom Nil) lst

sexp :: Parser Sexp
sexp = try atom <|> try pair <|> list

quote :: Parser Expression
quote =
  try (char '\'' >> fmap Quote sexp) <|>
  L.parens (L.reserved "quote" >> fmap Quote sexp)

binops :: Map.Map String Op
binops =
  Map.fromList
    [ ("add", Add)
    , ("sub", Sub)
    , ("mul", Mul)
    , ("sdiv", SDiv)
    , ("srem", SRem)
    , ("ilt", ILT)
    ]

binop'' :: String -> Parser Op
binop'' op = do
  L.reservedOp op
  case Map.lookup op binops of
    Nothing -> fail $ "unknown operator " ++ op
    Just op' -> return op'

binop' :: Parser Op
binop' =
  foldl (\parser op -> binop'' op <|> parser) parserZero (Map.keys binops)

binop :: Parser Expression
binop =
  L.parens $ do
    op <- binop'
    e1 <- expression
    e2 <- expression
    return $ BinOp op e1 e2

variable :: Parser Expression
variable = do
  var <- L.identifier
  return $ Variable var

lambda :: Parser Expression
lambda =
  L.parens $ do
    L.reserved "lambda"
    formals <- L.parens $ many L.identifier
    body <- expression
    return $ Lambda formals body

binding :: Parser Binding
binding =
  L.parens $ do
    var <- L.identifier
    init' <- expression
    return (var, init')

let' :: Parser Expression
let' = do
  bindings <- L.parens (many binding)
  body <- expression
  return $ Let bindings body

if' :: Parser Expression
if' =
  L.parens $ do
    L.reserved "if"
    test <- expression
    consequent <- expression
    alternate <- expression
    return $ If test consequent alternate

call :: Parser Expression
call =
  L.parens $ do
    (operator:operands) <- many1 expression
    return $ Call operator operands

case' :: Parser Expression
case' =
  L.parens $ do
    expr <- expression
    clauses <-
      many
        (L.parens $ do
           bind <-
             L.parens $ do
               name <- L.identifier
               t <- quasisexp
               return (name, t)
           e <- expression
           return (bind, e))
    return $ Case expr clauses

expression :: Parser Expression
expression =
  integer <|> try quote <|> try quasiquote <|> try binop <|> try variable <|>
  try lambda <|>
  try if' <|>
  try call

quasiatom :: Parser QuasiSexp
quasiatom = fmap QuasiAtom (fmap Integer L.integer <|> symbol)

quasipair :: Parser QuasiSexp
quasipair =
  L.parens $ do
    car <- many1 quasisexp
    L.reserved "."
    cdr <- quasisexp
    return $ foldr QuasiCons cdr car

quasinil :: Parser QuasiSexp
quasinil = return $ QuasiAtom Nil

quasilist :: Parser QuasiSexp
quasilist =
  L.parens $ do
    lst <- many quasisexp
    return $ foldr QuasiCons (QuasiAtom Nil) lst

quasisexp :: Parser QuasiSexp
quasisexp =
  quasiatom <|> try unquote <|> try unquoteSplicing <|> try quasipair <|>
  quasilist

unquoteSplicing :: Parser QuasiSexp
unquoteSplicing =
  try (char ',' >> char '@' >> fmap Expression expression) <|>
  L.parens (L.reserved "unquote-splicing" >> fmap Expression expression)

unquote :: Parser QuasiSexp
unquote =
  try (char ',' >> fmap Expression expression) <|>
  L.parens (L.reserved "unquote" >> fmap Expression expression)

quasiquote :: Parser Expression
quasiquote =
  try (char '`' >> fmap Quasiquote quasisexp) <|>
  L.parens (L.reserved "quasiquote" >> fmap Quasiquote quasisexp)

define :: Parser Top
define =
  L.parens $ do
    L.reserved "define"
    (name:formals) <- L.parens (many1 L.identifier)
    body <- expression
    return $ Define name formals body

declare :: Parser Top
declare =
  L.parens $ do
    L.reserved "declare"
    (name:types) <- L.parens (many1 L.identifier)
    return $ Declare name types

definition :: Parser Top
definition = try declare <|> define

command :: Parser Top
command = do
  cmd <- expression
  return $ Command cmd

program :: Parser Program
program = many $ try definition <|> command

contents :: Parser a -> Parser a
contents p = do
  Token.whiteSpace L.lexer
  r <- p
  eof
  return r

parseExpression :: String -> Either ParseError Expression
parseExpression = parse (contents expression) "<stdin>"

parseModule :: FilePath -> String -> Either ParseError Program
parseModule = parse (contents program)
