module Parser where

import Text.Parsec
import Text.Parsec.Char (char)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Token

import qualified Data.Map as Map

import qualified Lexer as L
import Syntax

integer :: Parser (Expression Id)
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

quote :: Parser (Expression Id)
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

binop :: Parser (Expression Id)
binop =
  L.parens $ do
    op <- binop'
    e1 <- expression
    e2 <- expression
    return $ BinOp op e1 e2

variable :: Parser (Expression Id)
variable = do
  pos <- getPosition
  var <- L.identifier
  return $ Variable (var, pos)

lambda :: Parser (Expression Id)
lambda =
  L.parens $ do
    L.reserved "lambda"
    formals <-
      L.parens $
      many1 $ do
        pos <- getPosition
        name <- L.identifier
        return (name, pos)
    body <- expression
    return $ Lambda formals body

binding :: Parser (Id, Expression Id)
binding =
  L.parens $ do
    pos <- getPosition
    var <- L.identifier
    init' <- expression
    return ((var, pos), init')

let' :: Parser (Expression Id)
let' = do
  bindings <- L.parens (many1 binding)
  body <- expression
  return $ Let bindings body

if' :: Parser (Expression Id)
if' =
  L.parens $ do
    L.reserved "if"
    test <- expression
    consequent <- expression
    alternate <- expression
    return $ If test consequent alternate

call :: Parser (Expression Id)
call =
  L.parens $ do
    (operator:operands) <- many1 expression
    return $ Call operator operands

case' :: Parser (Expression Id)
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

expression :: Parser (Expression Id)
expression =
  integer <|> try quote <|> try quasiquote <|> try binop <|> try variable <|>
  try lambda <|>
  try if' <|>
  try call

quasiatom :: Parser (Quasisexp Id)
quasiatom = fmap Quasiatom (fmap Integer L.integer <|> symbol)

quasipair :: Parser (Quasisexp Id)
quasipair =
  L.parens $ do
    car <- many1 quasisexp
    L.reserved "."
    cdr <- quasisexp
    return $ foldr Quasicons cdr car

quasinil :: Parser (Quasisexp Id)
quasinil = return $ Quasiatom Nil

quasilist :: Parser (Quasisexp Id)
quasilist =
  L.parens $ do
    lst <- many quasisexp
    return $ foldr Quasicons (Quasiatom Nil) lst

quasisexp :: Parser (Quasisexp Id)
quasisexp =
  quasiatom <|> try unquote <|> try unquoteSplicing <|> try quasipair <|>
  quasilist

unquoteSplicing :: Parser (Quasisexp Id)
unquoteSplicing =
  try (char ',' >> char '@' >> fmap UnquoteSplicing expression) <|>
  L.parens (L.reserved "unquote-splicing" >> fmap UnquoteSplicing expression)

unquote :: Parser (Quasisexp Id)
unquote =
  try (char ',' >> fmap Unquote expression) <|>
  L.parens (L.reserved "unquote" >> fmap Unquote expression)

quasiquote :: Parser (Expression Id)
quasiquote =
  try (char '`' >> fmap Quasiquote quasisexp) <|>
  L.parens (L.reserved "quasiquote" >> fmap Quasiquote quasisexp)

define :: Parser (Top Id)
define =
  L.parens $ do
    L.reserved "define"
    (name:formals) <-
      L.parens $
      many1 $ do
        pos <- getPosition
        name <- L.identifier
        return (name, pos)
    body <- expression
    return $ Define name formals body

declare :: Parser (Top Id)
declare =
  L.parens $ do
    L.reserved "declare"
    (name:types) <- L.parens (many1 L.identifier)
    return $ Declare name types

definition :: Parser (Top Id)
definition = try declare <|> define

command :: Parser (Top Id)
command = do
  cmd <- expression
  return $ Command cmd

program :: Parser (Program Id)
program = many $ try definition <|> command

contents :: Parser a -> Parser a
contents p = do
  Token.whiteSpace L.lexer
  r <- p
  eof
  return r

parseExpression :: String -> Either ParseError (Expression Id)
parseExpression = parse (contents expression) "<stdin>"

parseModule :: FilePath -> String -> Either ParseError (Program Id)
parseModule = parse (contents program)
