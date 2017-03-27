module Syntax where

type Name = String

type Program = [Top]

data Top
  = Define Name
           [Name]
           Expression
  | Declare Name
            [Name]
  | Command Expression
  deriving (Eq, Ord, Show)

type Binding = (Name, Expression)

type Bindings = [Binding]

data Expression
  = Quote Sexp
  | Quasiquote QuasiSexp
  | BinOp Op
          Expression
          Expression
  | Variable Name
  | Lambda [Name]
           Expression
  | Let Bindings
        Expression
  | If Expression
       Expression
       Expression
  | Call Expression
         [Expression]
  | Case Expression
         [((Name, QuasiSexp), Expression)]
  deriving (Eq, Ord, Show)

data QuasiSexp
  = QuasiAtom Atom
  | QuasiCons QuasiSexp
              QuasiSexp
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

data Op
  = Add
  | Sub
  | Mul
  | SDiv
  | SRem
  | ILT
  deriving (Eq, Ord, Show)
