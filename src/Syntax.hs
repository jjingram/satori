module Syntax where

import Text.Parsec.Pos

type Name = String

type Id = (Name, SourcePos)

type Env = [Name]

data Core
  = Name Name
  | Id (Name, SourcePos)
  | Ref (Name, Int, SourcePos)
  | Closure (Name, Env, SourcePos)
  deriving (Eq, Ord, Show)

coreId :: Id -> Core
coreId (name, pos) = Id (name, pos)

ref :: Id -> Int -> Core
ref (name, pos) idx = Ref (name, idx, pos)

closure :: Id -> Env -> Core
closure (name, pos) env = Closure (name, env, pos)

type Program a = [Top a]

data Top a
  = Define a
           [a]
           (Expression a)
  | Declare Name
            [Name]
  | Command (Expression a)
  deriving (Eq, Ord, Show)

data Expression a
  = Quote Sexp
  | Quasiquote (Quasisexp a)
  | BinOp Op
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
  | Case (Expression a)
         [((Name, Quasisexp a), Expression a)]
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

data Op
  = Add
  | Sub
  | Mul
  | SDiv
  | SRem
  | ILT
  deriving (Eq, Ord, Show)
