module Parser where

import Text.Parsec
import Text.Parsec.Char (char)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Token

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

variable :: Parser Expression
variable = do
  var <- L.identifier
  return $ Variable var

lambda :: Parser Expression
lambda =
  L.parens $ do
    L.reserved "lambda"
    formals <- many L.identifier
    body <- many expression
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
  body <- many expression
  return $ Let bindings body

if' :: Parser Expression
if' = do
  test <- expression
  consequent <- expression
  alternate <- expression
  return $ If test consequent alternate

call :: Parser Expression
call =
  L.parens $ do
    operator <- L.identifier
    operands <- many expression
    return $ Call operator operands

expression :: Parser Expression
expression = integer <|> try quote <|> try quasiquote <|> try variable <|> call

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

define :: Parser Toplevel
define =
  L.parens $ do
    L.reserved "define"
    (name:formals) <- L.parens (many1 L.identifier)
    body <- many expression
    return $ Define name formals body

declare :: Parser Toplevel
declare =
  L.parens $ do
    L.reserved "declare"
    (name:args) <- L.parens (many1 L.identifier)
    return $ Declare name args

definition :: Parser Toplevel
definition = try declare <|> define

command :: Parser Toplevel
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

parseProgram :: String -> Either ParseError Program
parseProgram = parse (contents program) "<stdin>"
