module Syntax where

import Text.Parsec.Pos

import Type

type Name = String

type Id = (Name, SourcePos)

type Env = [Name]

data Core
  = Name Name
  | Id (Name, Type, SourcePos)
  | Ref (Name, Type, Int, SourcePos)
  | Closure (Name, Type, Env, SourcePos)
  deriving (Eq, Ord, Show)

coreId :: Id -> Type -> Core
coreId (name, pos) ty = Id (name, ty, pos)

ref :: Id -> Type -> Int -> Core
ref (name, pos) ty idx = Ref (name, ty, idx, pos)

closure :: Id -> Type -> Env -> Core
closure (name, pos) ty env = Closure (name, ty, env, pos)

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
