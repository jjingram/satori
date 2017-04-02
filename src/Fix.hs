module Fix where

import Syntax

fixTop :: Program Name -> Program Name
fixTop (Define name params expr:rest) =
  if isRecursive name expr
    then Define name params (Fix name expr) : fixTop rest
    else Define name params expr : fixTop rest
fixTop (top:rest) = top : fixTop rest
fixTop [] = []

isRecursive :: Name -> Expression Name -> Bool
isRecursive name expr =
  case expr of
    Quote _ -> False
    Quasiquote sexp -> isRecursive' sexp
      where isRecursive' :: Quasisexp Name -> Bool
            isRecursive' s =
              case s of
                Quasiatom _ -> False
                Quasicons car cdr -> isRecursive' car || isRecursive' cdr
                Unquote x -> isRecursive name x
                UnquoteSplicing x -> isRecursive name x
    BinOp _ e1 e2 -> isRecursive name e1 || isRecursive name e2
    Variable x -> x == name
    Lambda _ e -> isRecursive name e
    Let bindings e ->
      foldl (\acc x -> isRecursive name x || acc) False exprs ||
      isRecursive name e
      where (_, exprs) = unzip bindings
    If cond tr fl ->
      isRecursive name cond || isRecursive name tr || isRecursive name fl
    Call f args ->
      isRecursive name f ||
      foldl (\acc x -> isRecursive name x || acc) False args
    Case e alts ->
      foldl (\acc x -> isRecursive name x || acc) False exprs ||
      isRecursive name e
      where (_, exprs) = unzip alts
