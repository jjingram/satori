module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Token

import qualified Lexer as Lexer
import Syntax

integer :: Parser Expression
integer = do
  n <- Lexer.integer
  return $ Number n

variable :: Parser Expression
variable = do
  var <- Lexer.identifier
  return $ Variable var

definition :: Parser Expression
definition = do
  Lexer.reserved "define"
  (name:args) <- Lexer.parens (many1 Lexer.identifier)
  body <- expression
  return $ Definition name args body

declaration :: Parser Expression
declaration = do
  Lexer.reserved "declare"
  (name:args) <- Lexer.parens (many1 Lexer.identifier)
  return $ Declaration name args

call :: Parser Expression
call = do
  name <- Lexer.identifier
  args <- many expression
  return $ Call name args

expression :: Parser Expression
expression =
  try integer <|> try variable <|>
  Lexer.parens (try definition <|> try declaration <|> call)

contents :: Parser a -> Parser a
contents p = do
  Token.whiteSpace Lexer.lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expression]
toplevel = many expression

parseExpression :: String -> Either ParseError Expression
parseExpression s = parse (contents expression) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expression]
parseToplevel s = parse (contents toplevel) "<stdin>" s
