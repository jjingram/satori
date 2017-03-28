module Curry where

import Prelude hiding (curry)

import Syntax

curryTop :: Top Id -> Top Id
curryTop (Define name formals body) =
  Define name [] (curry (Lambda formals body))
curryTop (Declare name types) = Declare name types
curryTop (Command expr) = Command (curry expr)

curry :: Expression Id -> Expression Id
curry x@(Quote _) = x
curry (Quasiquote x) = Quasiquote $ qq x
curry (BinOp op e1 e2) = BinOp op (curry e1) (curry e2)
curry x@(Variable _) = x
curry (Lambda xs e) = foldr (\x e' -> Lambda [x] e') (curry e) xs
curry (Let bindings body) =
  foldr (\(x, e1) e2 -> Let [(x, curry e1)] e2) (curry body) bindings
curry (If cond tr fl) = If (curry cond) (curry tr) (curry fl)
curry (Call operator operands) =
  foldl (\x e -> Call x [e]) (curry operator) (map curry operands)
curry (Syntax.Case x clauses) = Case (curry x) (zip (zip names types') bodies')
  where
    (bindings, bodies) = unzip clauses
    (names, types) = unzip bindings
    types' = map qq types
    bodies' = map curry bodies

qq :: Quasisexp Id -> Quasisexp Id
qq x@(Quasiatom _) = x
qq (Quasicons car cdr) = Quasicons (qq car) (qq cdr)
qq (UnquoteSplicing e) = UnquoteSplicing $ curry e
qq (Unquote e) = Unquote $ curry e

definitions :: Program Id -> [(Id, Expression Id)]
definitions [] = []
definitions (Define id [] expr:rest) = (id, expr) : definitions rest
definitions (_:rest) = definitions rest

without
  :: Eq a
  => [a] -> [a] -> [a]
without = foldr (filter . (/=))

free :: Expression Id -> [Name]
free (Quote _) = []
free (Quasiquote x) = vars x
free (BinOp _ e1 e2) = free e1 ++ free e2
free (Variable x) = [name]
  where
    (name, _) = x
free (Lambda x e) = free e `without` names
  where
    (names, _) = unzip x
free (Let binds e2) = concatMap free es ++ (free e2 `without` names)
  where
    (xs, es) = unzip binds
    (names, _) = unzip xs
free (If cond tr fl) = free cond ++ free tr ++ free fl
free (Call e1 e2) = free e1 ++ concatMap free e2
free (Case x clauses) = free x ++ types' ++ bodies'
  where
    (bindings, bodies) = unzip clauses
    (_, types) = unzip bindings
    types' = concatMap vars types
    bodies' = concatMap free bodies

vars :: Quasisexp Id -> [Name]
vars (Quasiatom _) = []
vars (Quasicons car cdr) = vars car ++ vars cdr
vars (Unquote e) = free e
vars (UnquoteSplicing e) = free e
