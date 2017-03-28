module Curry where

import Prelude hiding (curry)

import Syntax

curryTop :: Top Name -> Top Name
curryTop (Define name formals body) =
  Define name [] (curry (Lambda formals body))
curryTop (Declare name types) = Declare name types
curryTop (Command expr) = Command (curry expr)

curry :: Expression Name -> Expression Name
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
curry (Syntax.Case x clauses) = Case (curry x) (zip (zip names types) bodies')
  where
    (bindings, bodies) = unzip clauses
    (names, types) = unzip bindings
    bodies' = map curry bodies
curry (Fix x) = Fix (curry x)

qq :: Quasisexp Name -> Quasisexp Name
qq x@(Quasiatom _) = x
qq (Quasicons car cdr) = Quasicons (qq car) (qq cdr)
qq (UnquoteSplicing e) = UnquoteSplicing $ curry e
qq (Unquote e) = Unquote $ curry e

definitions :: Program Name -> [(Name, Expression Name)]
definitions [] = []
definitions (Define name [] expr:rest) = (name, expr) : definitions rest
definitions (_:rest) = definitions rest
