module Closure where

import Data.List

import Core
import Infer
import Pretty ()

type Name = String

convert' :: Free -> Free -> Quasisexp Typed -> Quasisexp Typed
convert' env fvs expr =
  case expr of
    (Quasiatom x) -> Quasiatom x
    (Quasicons car cdr) ->
      Quasicons (convert' env fvs car) (convert' env fvs cdr)
    (Unquote e) -> Unquote $ convert env fvs e
    (UnquoteSplicing e) -> UnquoteSplicing $ convert env fvs e

convert :: Free -> Free -> Expression Typed -> Expression Typed
convert env fvs expr =
  case expr of
    (Quote x) -> Quote x
    (Quasiquote x) -> Quasiquote $ convert' env fvs x
    (BinOp op e1 e2) -> BinOp op (convert env fvs e1) (convert env fvs e2)
    (Variable x) -> Variable x
    (Lambda n x t _ e) -> Lambda n x t fvs' (convert env fvs' e)
      where x' = head x
            fvs' = sort $ nub $ free e `without` (x' : env) ++ fvs
    (Let b e2) -> Let [(x, convert env' fvs' e1)] (convert env' fvs' e2)
      where (x, e1) = head b
            env' = x : env
            fvs' = fvs `without` [x]
    (If cond tr fl) ->
      If (convert env fvs cond) (convert env fvs tr) (convert env fvs fl)
    (Call e1 e2) -> Call (convert env fvs e1) (map (convert env fvs) e2)
    (Case e clauses) -> Case (convert env fvs e) clauses'
      where (bindings, bodies) = unzip clauses
            bodies' = map (convert env fvs) bodies
            clauses' = zip bindings bodies'
    (Fix e) -> Fix (convert env fvs e)

without
  :: Eq a
  => [a] -> [a] -> [a]
without = foldr (filter . (/=))

free :: Expression Typed -> [Typed]
free (Quote _) = []
free (Quasiquote x) = free' x
free (BinOp _ e1 e2) = free e1 ++ free e2
free (Variable x) = [x]
free (Lambda _ x _ _ e) = free e `without` x
free (Let binds e2) = concatMap free es ++ (free e2 `without` xs)
  where
    (xs, es) = unzip binds
free (If cond tr fl) = free cond ++ free tr ++ free fl
free (Call e1 e2) = free e1 ++ concatMap free e2
free (Case x clauses) = (free x ++ bodies') `without` bindings'
  where
    (bindings, bodies) = unzip clauses
    (names, patterns) = unzip bindings
    types = map inferQuote patterns
    bindings' = zip names types
    bodies' = concatMap free bodies
free (Fix x) = free x

free' :: Quasisexp Typed -> [Typed]
free' (Quasiatom _) = []
free' (Quasicons car cdr) = free' car ++ free' cdr
free' (Unquote e) = free e
free' (UnquoteSplicing e) = free e
