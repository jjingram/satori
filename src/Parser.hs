module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Token

import qualified Lexer
import Syntax

integer :: Parser Expression
integer = do
  n <- Lexer.integer
  return $ Number n

variable :: Parser Expression
variable = do
  var <- Lexer.identifier
  return $ Variable var

call :: Parser Expression
call = do
  name <- Lexer.identifier
  args <- many expression
  return $ Call name args

expression :: Parser Expression
expression = integer <|> variable <|> Lexer.parens call

define :: Parser Toplevel
define = do
  Lexer.reserved "define"
  (name:args) <- Lexer.parens (many1 Lexer.identifier)
  body <- expression
  return $ Define name args body

declare :: Parser Toplevel
declare = do
  Lexer.reserved "declare"
  (name:args) <- Lexer.parens (many1 Lexer.identifier)
  return $ Declare name args

definition :: Parser Toplevel
definition = Lexer.parens (declare <|> define)

command :: Parser Toplevel
command = do
  x <- expression
  return $ Command x

contents :: Parser a -> Parser a
contents p = do
  Token.whiteSpace Lexer.lexer
  r <- p
  eof
  return r

program :: Parser Program
program = many $ try definition <|> command

parseProgram :: String -> Either ParseError Program
parseProgram = parse (contents program) "<stdin>"
