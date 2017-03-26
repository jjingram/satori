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
    return $ foldr Cons Nil lst

sexp :: Parser Sexp
sexp = try atom <|> try pair <|> list

quote :: Parser Expression
quote =
  (try (L.reserved "'") >> fmap Quote sexp) <|>
  L.parens (L.reserved "quote" >> fmap Quote sexp)

variable :: Parser Expression
variable = do
  var <- L.identifier
  return $ Variable var

call :: Parser Expression
call =
  L.parens $ do
    name <- L.identifier
    args <- many expression
    return $ Call name args

expression :: Parser Expression
expression = integer <|> try quote <|> try variable <|> call

define :: Parser Toplevel
define =
  L.parens $ do
    L.reserved "define"
    (name:args) <- L.parens (many1 L.identifier)
    body <- expression
    return $ Define name args body

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
