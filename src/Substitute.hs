module Substitute where

import qualified Data.Map as Map

import Syntax

type Subs = Map.Map Name (Expression Name)

substitute :: Subs -> Program Name -> Program Name
substitute _ [] = []
substitute subs (top:rest) =
  case top of
    (Define name formals expr) ->
      Define name formals (substitute' subs expr) : substitute subs rest
    (Declare _ _) -> substitute subs rest
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
        Let b e2 ->
          if val
            then substitute' subs'' e2
            else Let [b'] (substitute' subs'' e2)
          where (name, e1) = head b
                e1' = substitute' subs' e1
                b' = (name, e1')
                val = isval e1'
                subs'' =
                  if val
                    then Map.insert name e1' subs'
                    else subs'
        If cond tr fl ->
          If
            (substitute' subs' cond)
            (substitute' subs' tr)
            (substitute' subs' fl)
        Call e1 e2 -> Call (substitute' subs' e1) [substitute' subs' e2']
          where e2' = head e2
        Case e alts -> Case (substitute' subs' e) alts
        Fix e -> Fix (substitute' subs' e)
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
isval Case {} = False
