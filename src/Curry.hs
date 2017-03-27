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
qq (Expression e) = Expr $ curry e

without
  :: Eq a
  => [a] -> [a] -> [a]
without = foldr (filter . (/=))

fv :: Expr -> [Name]
fv (Q _) = []
fv (QQ x) = vars x
fv (Curry.BinOp _ e1 e2) = fv e1 ++ fv e2
fv (Var x) = [x]
fv (Lam x e) = fv e `without` [x]
fv (Curry.Let x e1 e2) = fv e1 ++ (fv e2 `without` [x])
fv (Curry.If p tr fl) = fv p ++ fv tr ++ fv fl
fv (App e1 e2) = fv e1 ++ fv e2
fv (Fix e) = fv e
fv (Curry.Case x clauses) = fv x ++ types' ++ bodies'
  where
    (bindings, bodies) = unzip clauses
    (_, types) = unzip bindings
    types' = concatMap vars types
    bodies' = concatMap fv bodies

vars :: QSexp -> [Name]
vars (QAtom _) = []
vars (QCons car cdr) = vars car ++ vars cdr
vars (Expr e) = fv e

appliedTo :: Expr -> [Name] -> Expr
appliedTo = foldl (\e a -> (App e $ Var a))

convertQQ :: [Name] -> QSexp -> QSexp
convertQQ env expr =
  case expr of
    x@(QAtom _) -> x
    (QCons car cdr) -> QCons (convertQQ env car) (convertQQ env cdr)
    (Expr e) -> Expr $ convert env e

convert :: [Name] -> Expr -> Expr
convert env expr =
  case expr of
    x@(Q _) -> x
    (QQ x) -> QQ $ convertQQ env x
    (Curry.BinOp op e1 e2) -> Curry.BinOp op (convert env e1) (convert env e2)
    x@(Var _) -> x
    (Lam x e) -> lam `appliedTo` args
      where lam = foldr Lam e (args ++ [x])
            args = fv e `without` (x : env)
    (Curry.Let x e1 e2) -> Curry.Let x (convert env' e1) (convert env' e2)
      where env' = x : env
    (Curry.If t tr fl) ->
      Curry.If (convert env t) (convert env tr) (convert env fl)
    (App e1 e2) -> App (convert env e1) (convert env e2)
    (Fix e) -> convert env e
    (Curry.Case e clauses) -> Curry.Case (convert env e) clauses'
      where (bindings, bodies) = unzip clauses
            bodies' = map (convert env) bodies
            clauses' = zip bindings bodies'

definitions :: Curry.Program -> [(String, Expr)]
definitions [] = []
definitions (Def name expr:rest) = (name, expr) : definitions rest
definitions (_:rest) = definitions rest
