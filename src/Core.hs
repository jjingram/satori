module Core where

import Type

type Name = String

data Expression
  = App Expression
        Expression
  | Variable Variable
  | Lambda Name
           Type
           Expression
  | Case Expression
         [Alternative]
  | Let Bind
        Expression
  | Quote Sexp
  | Quasiquote Quasisexp
  deriving (Eq, Ord, Show)

data Variable =
  Id Name
     Type
  deriving (Eq, Ord, Show)

data Alternative =
  Alternative Name
              Quasisexp
              Expression
  deriving (Eq, Ord, Show)

data Bind =
  Bind Variable
       Expression
  deriving (Eq, Ord, Show)

data Quasisexp
  = Quasiatom Atom
  | Quasicons Quasisexp
              Quasisexp
  | Unquote Expression
  | UnquoteSplicing Expression
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
