module Substitute where

import qualified Data.Map as Map

import Syntax

type Subs = Map.Map Name (Expression Name)

substitute :: Subs -> Program Name -> Program Name
substitute _ [] = []
substitute subs (top:rest) =
  case top of
    (Define name expr) ->
      let expr' = substitute' (Map.delete name subs) expr
      in let expr'' =
               if isRecursive name expr'
                 then Fix name expr'
                 else expr'
         in Define name expr'' : substitute (Map.insert name expr'' subs) rest
    top'@Declare {} -> top' : substitute subs rest
    (Command expr) -> Command (substitute' subs expr) : substitute subs rest
  where
    substitute' :: Subs -> Expression Name -> Expression Name
    substitute' subs' expr =
      case expr of
        Quote sexp -> Quote sexp
        Quasiquote sexp -> Quasiquote (substitute'' subs' sexp)
        BinOp op e1 e2 -> BinOp op (substitute' subs' e1) (substitute' subs' e2)
        Variable x -> Map.findWithDefault (Variable x) x subs'
        Lambda xs e -> Lambda xs (substitute' subs' e)
        Let bindings body ->
          if null bindings'
            then body'
            else Let bindings' body'
          where (bindings', body') = substituteBindings subs bindings [] body
                substituteBindings
                  :: Subs
                  -> [(Name, Expression Name)]
                  -> [(Name, Expression Name)]
                  -> Expression Name
                  -> ([(Name, Expression Name)], Expression Name)
                substituteBindings _ [] acc b = (acc, b)
                substituteBindings s ((name, e):rest') acc b =
                  if isval e''
                    then substituteBindings s' rest' acc (substitute' s' b)
                    else substituteBindings s' rest' (acc ++ [(name, e)]) b
                  where
                    e' =
                      if isRecursive name e
                        then Fix name e
                        else e
                    e'' = substitute' s e'
                    s' =
                      if isval e''
                        then Map.insert name e'' s
                        else s
        If cond tr fl ->
          If
            (substitute' subs' cond)
            (substitute' subs' tr)
            (substitute' subs' fl)
        Call e1 e2 -> Call (substitute' subs' e1) (map (substitute' subs) e2)
        Fix name e -> Fix name (substitute' subs' e)
    substitute'' :: Subs -> Quasisexp Name -> Quasisexp Name
    substitute'' subs' sexp =
      case sexp of
        Quasiatom x -> Quasiatom x
        Quasicons car cdr ->
          Quasicons (substitute'' subs' car) (substitute'' subs' cdr)
        Unquote e -> Unquote (substitute' subs' e)
        UnquoteSplicing e -> UnquoteSplicing (substitute' subs' e)

isval :: Expression Name -> Bool
isval Quote {} = True
isval (Quasiquote sexp) = isval' sexp
  where
    isval' :: Quasisexp Name -> Bool
    isval' Quasiatom {} = True
    isval' (Quasicons car cdr) = isval' car && isval' cdr
    isval' (Unquote x) = isval x
    isval' (UnquoteSplicing x) = isval x
isval (BinOp _ e1 e2) = isval e1 && isval e2
isval Variable {} = False
isval Lambda {} = True
isval Let {} = False
isval If {} = False
isval Call {} = False
isval (Fix _ e) = isval e

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
    Fix _ e -> isRecursive name e
