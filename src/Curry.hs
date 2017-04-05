module Curry where

import Prelude hiding (curry)

import Syntax

curryTop :: Top Name -> Top Name
curryTop (Define name body) = Define name (curry body)
curryTop (Declare name tys) = Declare name tys
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
curry (Fix name e) = Fix name (curry e)

qq :: Quasisexp Name -> Quasisexp Name
qq x@(Quasiatom _) = x
qq (Quasicons car cdr) = Quasicons (qq car) (qq cdr)
qq (UnquoteSplicing e) = UnquoteSplicing $ curry e
qq (Unquote e) = Unquote $ curry e
