module Syntax where

type Name = String

type Program = [Toplevel]

data Toplevel
  = Define Name
           [Name]
           Expression
  | Declare Name
            [Name]
  | Command Expression
  deriving (Eq, Ord, Show)

data Expression
  = Quote Sexp
  | Variable Name
  | Call Name
         [Expression]
  deriving (Eq, Ord, Show)

data Sexp
  = Nil
  | Atom Atom
  | Cons Sexp
         Sexp
  deriving (Eq, Ord, Show)

data Atom
  = Integer Integer
  | Symbol Name
  deriving (Eq, Ord, Show)
