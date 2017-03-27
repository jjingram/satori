module Curry where

import Prelude hiding (curry)

import Syntax

type Program = [Curry.Top]

data Top
  = Def Name
        Expr
  | Decl Name
         [Name]
  | Cmd Expr
  deriving (Eq, Ord, Show)

data Expr
  = Q Sexp
  | QQ QSexp
  | BinOp Op
          Expr
          Expr
  | Var Name
  | Lam Name
        Expr
  | Let Name
        Expr
        Expr
  | If Expr
       Expr
       Expr
  | App Expr
        Expr
  | Fix Expr
  | Case Expr
         [((Name, QSexp), Expr)]
  deriving (Eq, Ord, Show)

data QSexp
  = QAtom Atom
  | QCons QSexp
          QSexp
  | Expr Expr
  deriving (Eq, Ord, Show)

curryTop :: Syntax.Top -> Curry.Top
curryTop (Syntax.Define name formals body) =
  Curry.Define name (curry (Lambda formals body))
curryTop (Syntax.Declare name types) = Curry.Declare name types
curryTop (Syntax.Command expr) = Curry.Command (curry expr)

curry :: Expression -> Expr
curry (Quote sexp) = Q sexp
curry (Quasiquote x) = QQ $ qq x
curry (Syntax.BinOp op e1 e2) = Curry.BinOp op (curry e1) (curry e2)
curry (Variable name) = Var name
curry (Lambda formals body) = foldr Lam (curry body) formals
curry (Syntax.Let bindings body) =
  foldr (\(x, e1) e2 -> Curry.Let x (curry e1) e2) (curry body) bindings
curry (Syntax.If test consequent alternative) =
  Curry.If (curry test) (curry consequent) (curry alternative)
curry (Call operator operands) = foldl App (curry operator) (map curry operands)
curry (Syntax.Case x clauses) =
  Curry.Case (curry x) (zip (zip names types') bodies')
  where
    (bindings, bodies) = unzip clauses
    (names, types) = unzip bindings
    types' = map qq types
    bodies' = map curry bodies

qq :: QuasiSexp -> QSexp
qq (QuasiAtom x) = QAtom x
qq (QuasiCons car cdr) = QCons (qq car) (qq cdr)
qq (Expression e) = Expr $ curry e
