module Syntax where

type Name = String

type Program = [Toplevel]

type Body = [Expression]

data Toplevel
  = Define Name
           [Name]
           Body
  | Declare Name
            [Name]
  | Command Expression
  deriving (Eq, Ord, Show)

type Binding = (Name, Expression)

type Bindings = [Binding]

data Expression
  = Quote Sexp
  | Quasiquote QuasiSexp
  | Variable Name
  | Lambda [Name]
           Body
  | Let Bindings
        Body
  | If Expression
       Expression
       Expression
  | Call Name
         [Expression]
  | Case Expression
         [(QuasiSexp, Expression)]
  deriving (Eq, Ord, Show)

data QuasiSexp
  = QuasiAtom Atom
  | QuasiCons QuasiSexp
              QuasiSexp
  | Expression Expression
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
