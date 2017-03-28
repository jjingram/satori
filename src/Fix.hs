module Fix where

import Syntax

fix' :: Program Name -> Program Name
fix' (Define name params expr:rest) =
  Define name params (fix'' name expr) : fix' rest
fix' (_:rest) = fix' rest
fix' [] = []

fix'' :: Name -> Expression Name -> Expression Name
fix'' name expr =
  case expr of
    Quote sexp -> Quote sexp
    Quasiquote sexp -> Quasiquote $ fix''' sexp
      where fix''' :: Quasisexp Name -> Quasisexp Name
            fix''' s =
              case s of
                Quasiatom x -> Quasiatom x
                Quasicons car cdr -> Quasicons (fix''' car) (fix''' cdr)
                Unquote x -> Unquote $ fix'' name x
                UnquoteSplicing x -> UnquoteSplicing $ fix'' name x
    BinOp op e1 e2 -> BinOp op (fix'' name e1) (fix'' name e2)
    Variable x ->
      if x == name
        then Fix $ Variable x
        else Variable x
    Lambda params e -> Lambda params (fix'' name e)
    Let bindings e -> Let bindings' (fix'' name e)
      where (names, exprs) = unzip bindings
            exprs' = map (fix'' name) exprs
            bindings' = zip names exprs'
    If cond tr fl -> If (fix'' name cond) (fix'' name tr) (fix'' name fl)
    Call f args -> Call (fix'' name f) (map (fix'' name) args)
    Case e alts -> Case (fix'' name e) alts'
      where (binds, exprs) = unzip alts
            exprs' = map (fix'' name) exprs
            alts' = zip binds exprs'
    Fix e -> Fix (fix'' name e)
