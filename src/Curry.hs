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
  | UQ Expr
  | UQS Expr
  deriving (Eq, Ord, Show)

curryTop :: Syntax.Top -> Curry.Top
curryTop (Define name formals body) = Def name (curry (Lambda formals body))
curryTop (Declare name types) = Decl name types
curryTop (Command expr) = Cmd (curry expr)

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
qq (UnquoteSplicing e) = UQS $ curry e
qq (Unquote e) = UQ $ curry e

definitions :: Curry.Program -> [(String, Expr)]
definitions [] = []
definitions (Def name expr:rest) = (name, expr) : definitions rest
definitions (_:rest) = definitions rest

without
  :: Eq a
  => [a] -> [a] -> [a]
without = foldr (filter . (/=))

free :: Expr -> [Name]
free (Q _) = []
free (QQ x) = vars x
free (Curry.BinOp _ e1 e2) = free e1 ++ free e2
free (Var x) = [x]
free (Lam x e) = free e `without` [x]
free (Curry.Let x e1 e2) = free e1 ++ (free e2 `without` [x])
free (Curry.If p tr fl) = free p ++ free tr ++ free fl
free (App e1 e2) = free e1 ++ free e2
free (Fix e) = free e
free (Curry.Case x clauses) = free x ++ types' ++ bodies'
  where
    (bindings, bodies) = unzip clauses
    (_, types) = unzip bindings
    types' = concatMap vars types
    bodies' = concatMap free bodies

vars :: QSexp -> [Name]
vars (QAtom _) = []
vars (QCons car cdr) = vars car ++ vars cdr
vars (UQ e) = free e
vars (UQS e) = free e
