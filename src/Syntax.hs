module Syntax where

import qualified Data.Map as Map

import Type

type Name = String

type Typed = (Name, Type)

type Program a = [Top a]

data Top a
  = Define a
           (Expression a)
  | Declare Name
            [Type]
  | Command (Expression a)
  deriving (Eq, Ord, Show)

data Expression a
  = Quote Sexp
  | Quasiquote (Quasisexp a)
  | BinOp BinOp
          (Expression a)
          (Expression a)
  | Variable a
  | Lambda [a]
           (Expression a)
  | Let [(a, Expression a)]
        (Expression a)
  | If (Expression a)
       (Expression a)
       (Expression a)
  | Call (Expression a)
         [Expression a]
  | Fix a
        (Expression a)
  deriving (Eq, Ord, Show)

data Quasisexp a
  = Quasiatom Atom
  | Quasicons (Quasisexp a)
              (Quasisexp a)
  | Unquote (Expression a)
  | UnquoteSplicing (Expression a)
  deriving (Eq, Ord, Show)

data Sexp
  = Atom Atom
  | Cons Sexp
         Sexp
  deriving (Eq, Ord, Show)

data Atom
  = Nil
  | Integer Integer
  | Symbol Name
  deriving (Eq, Ord, Show)

data BinOp
  = Add
  | Sub
  | Mul
  | SDiv
  | SRem
  | SLT
  | EQ
  deriving (Eq, Ord, Show)

binops :: Map.Map BinOp (Name, Type)
binops =
  Map.fromList
    [ (Add, ("add", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (Mul, ("mul", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (Sub, ("sub", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (SDiv, ("sdiv", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (SRem, ("srem", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (SLT, ("slt", i64 `TypeArrow` (i64 `TypeArrow` i1)))
    , (Syntax.EQ, ("eq", i64 `TypeArrow` (i64 `TypeArrow` i1)))
    ]

filterDefinitions :: Program Typed -> Program Typed
filterDefinitions [] = []
filterDefinitions (Define {}:rest) = filterDefinitions rest
filterDefinitions (x:rest) = x : filterDefinitions rest

definitions :: Program Name -> [(Name, Expression Name)]
definitions [] = []
definitions (Define name expr:rest) = (name, expr) : definitions rest
definitions (_:rest) = definitions rest

definitions' :: Program Typed -> [(Name, Top Typed)]
definitions' [] = []
definitions' (top@(Define (name, _) _):rest) = (name, top) : definitions' rest
definitions' (_:rest) = definitions' rest
