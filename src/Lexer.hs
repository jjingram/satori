module Lexer where

import Text.Parsec ((<|>), letter, digit)
import Text.Parsec.Char (oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Token

symbol :: Parser Char
symbol = oneOf "!$%&*+-./:<=>?@^_~"

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    names =
      [ "cons"
      , "."
      , "quote"
      , "'"
      , "quasiquote"
      , "`"
      , "unquote"
      , ","
      , "unquote-splicing"
      , ",@"
      , "let"
      , "lambda"
      , "define"
      , "declare"
      ]
    ops = ["add", "sub", "mul", "sdiv", "srem", "ilt"]
    style =
      emptyDef
      { Token.commentStart = "#|"
      , Token.commentEnd = "|#"
      , Token.commentLine = ";"
      , Token.nestedComments = True
      , Token.identStart = letter <|> symbol
      , Token.identLetter = letter <|> symbol <|> digit
      , Token.reservedNames = names
      , Token.reservedOpNames = ops
      , Token.caseSensitive = True
      }

integer :: Parser Integer
integer = Token.integer lexer

identifier :: Parser String
identifier = Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer
