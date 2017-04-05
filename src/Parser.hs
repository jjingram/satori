module Parser
  ( parseExpression
  , parseModule
  ) where

import Text.Parsec
import Text.Parsec.Char (char)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Token

import qualified Data.Map as Map

import qualified Lexer as L
import Syntax
import Type

integer :: Parser (Expression Name)
integer = do
  n <- L.integer
  return $ Quote $ Atom $ Integer n

nil :: Parser Atom
nil = do
  L.reserved "nil"
  return Nil

symbol :: Parser Atom
symbol = do
  s <- L.identifier
  return $ Symbol s

atom :: Parser Sexp
atom = fmap Atom (fmap Integer L.integer <|> symbol <|> Parser.nil)

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

quote :: Parser (Expression Name)
quote =
  try (char '\'' >> fmap Quote sexp) <|>
  L.parens (L.reserved "quote" >> fmap Quote sexp)

binops :: Map.Map String BinOp
binops =
  Map.fromList
    [ ("add", Add)
    , ("sub", Sub)
    , ("mul", Mul)
    , ("sdiv", SDiv)
    , ("srem", SRem)
    , ("slt", SLT)
    , ("eq", Syntax.EQ)
    ]

binop'' :: String -> Parser BinOp
binop'' op = do
  L.reservedOp op
  case Map.lookup op Parser.binops of
    Nothing -> fail $ "unknown operator " ++ op
    Just op' -> return op'

binop' :: Parser BinOp
binop' =
  foldl
    (\parser op -> binop'' op <|> parser)
    parserZero
    (Map.keys Parser.binops)

binop :: Parser (Expression Name)
binop =
  L.parens $ do
    op <- binop'
    e1 <- expression
    e2 <- expression
    return $ BinOp op e1 e2

variable :: Parser (Expression Name)
variable = do
  var <- L.identifier
  return $ Variable var

lambda :: Parser (Expression Name)
lambda =
  L.parens $ do
    L.reserved "lambda"
    formals <- L.parens $ many1 L.identifier
    body <- expression
    return $ Lambda formals body

binding :: Parser (Name, Expression Name)
binding =
  L.parens $ do
    name <- L.identifier
    init' <- expression
    return (name, init')

let' :: Parser (Expression Name)
let' =
  L.parens $ do
    L.reserved "let"
    bindings <- L.parens (many1 binding)
    body <- expression
    return $ Let bindings body

if' :: Parser (Expression Name)
if' =
  L.parens $ do
    L.reserved "if"
    test <- expression
    consequent <- expression
    alternate <- expression
    return $ If test consequent alternate

call :: Parser (Expression Name)
call =
  L.parens $ do
    (operator:operands) <- many1 expression
    return $ Call operator operands

expression :: Parser (Expression Name)
expression =
  integer <|> try quote <|> try quasiquote <|> try binop <|> try variable <|>
  try Parser.lambda <|>
  try let' <|>
  try if' <|>
  call

quasiatom :: Parser (Quasisexp Name)
quasiatom = fmap Quasiatom (fmap Integer L.integer <|> symbol <|> Parser.nil)

quasipair :: Parser (Quasisexp Name)
quasipair =
  L.parens $ do
    car <- many1 quasisexp
    L.reserved "."
    cdr <- quasisexp
    return $ foldr Quasicons cdr car

quasilist :: Parser (Quasisexp Name)
quasilist =
  L.parens $ do
    lst <- many quasisexp
    return $ foldr Quasicons (Quasiatom Nil) lst

quasisexp :: Parser (Quasisexp Name)
quasisexp =
  quasiatom <|> try unquote <|> try unquoteSplicing <|> try quasipair <|>
  quasilist

unquoteSplicing :: Parser (Quasisexp Name)
unquoteSplicing =
  try (char ',' >> char '@' >> fmap UnquoteSplicing expression) <|>
  L.parens (L.reserved "unquote-splicing" >> fmap UnquoteSplicing expression)

unquote :: Parser (Quasisexp Name)
unquote =
  try (char ',' >> fmap Unquote expression) <|>
  L.parens (L.reserved "unquote" >> fmap Unquote expression)

quasiquote :: Parser (Expression Name)
quasiquote =
  try (char '`' >> fmap Quasiquote quasisexp) <|>
  L.parens (L.reserved "quasiquote" >> fmap Quasiquote quasisexp)

define :: Parser (Top Name)
define =
  L.parens $ do
    L.reserved "define"
    name <- L.identifier
    body <- expression
    return $ Define name body

declare :: Parser (Top Name)
declare =
  L.parens $ do
    L.reserved "declare"
    (name:types) <- L.parens (many1 L.identifier)
    return $ Declare name (map TypeSymbol types)

definition :: Parser (Top Name)
definition = try declare <|> define

command :: Parser (Top Name)
command = do
  cmd <- expression
  return $ Command cmd

program :: Parser (Program Name)
program = many $ try definition <|> command

contents :: Parser a -> Parser a
contents p = do
  Token.whiteSpace L.lexer
  r <- p
  eof
  return r

parseExpression :: String -> Either ParseError (Expression Name)
parseExpression = parse (contents expression) "<stdin>"

parseModule :: FilePath -> String -> Either ParseError (Program Name)
parseModule = parse (contents program)
