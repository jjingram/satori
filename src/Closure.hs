module Closure where

import Data.List
import Data.Maybe

import Curry
import Syntax

convert' :: [Name] -> [Name] -> Quasisexp Id -> Quasisexp Core
convert' env fvs expr =
  case expr of
    (Quasiatom x) -> Quasiatom x
    (Quasicons car cdr) ->
      Quasicons (convert' env fvs car) (convert' env fvs cdr)
    (Unquote e) -> Unquote $ convert env fvs e
    (UnquoteSplicing e) -> UnquoteSplicing $ convert env fvs e

convert :: [Name] -> [Name] -> Expression Id -> Expression Core
convert env fvs expr =
  case expr of
    (Quote x) -> Quote x
    (Quasiquote x) -> Quasiquote $ convert' env fvs x
    (BinOp op e1 e2) -> BinOp op (convert env fvs e1) (convert env fvs e2)
    (Variable x) ->
      let (name, _) = x
      in if name `elem` fvs
           then Variable (ref x (fromJust (elemIndex name fvs)))
           else Variable (coreId x)
    (Lambda x e) -> Lambda [closure (head x) fvs'] (convert env fvs' e)
      where (name, _) = head x
            fvs' = sort $ nub $ free e `without` (name : env) ++ fvs
    (Let b e2) ->
      Let [((coreId x), convert env' fvs' e1)] (convert env' fvs' e2)
      where (x, e1) = head b
            (name, _) = x
            env' = name : env
            fvs' = fvs `without` [name]
    (If cond tr fl) ->
      If (convert env fvs cond) (convert env fvs tr) (convert env fvs fl)
    (Call e1 e2) -> Call (convert env fvs e1) (map (convert env fvs) e2)
    (Case e clauses) -> Case (convert env fvs e) clauses'
      where (bindings, bodies) = unzip clauses
            (names, types) = unzip bindings
            types' = map (convert' env fvs) types
            bindings' = zip names types'
            bodies' = map (convert env fvs) bodies
            clauses' = zip bindings' bodies'
