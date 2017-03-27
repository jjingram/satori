module Closure where

import Data.List
import Data.Maybe

import Curry
import Syntax

type Program = [Closure.Top]

type Env = [Name]

data Top
  = Def Name
        Name
        Closure.Expr
  | Decl Name
         [Name]
  | Cmd Closure.Expr
  deriving (Eq, Ord, Show)

data Expr
  = Q Sexp
  | QQ Closure.QSexp
  | BinOp Op
          Closure.Expr
          Closure.Expr
  | Var Name
  | Ref Int
  | Let Name
        Closure.Expr
        Closure.Expr
  | If Closure.Expr
       Closure.Expr
       Closure.Expr
  | App Closure.Expr
        Closure.Expr
  | Fix Closure.Expr
  | Case Closure.Expr
         [((Name, Closure.QSexp), Closure.Expr)]
  | Closure Name
            Closure.Expr
            Env
  deriving (Eq, Ord, Show)

data QSexp
  = QAtom Atom
  | QCons Closure.QSexp
          Closure.QSexp
  | UQ Closure.Expr
  | UQS Closure.Expr
  deriving (Eq, Ord, Show)

convertQQ :: [Name] -> [Name] -> Curry.QSexp -> Closure.QSexp
convertQQ env fvs expr =
  case expr of
    (Curry.QAtom x) -> Closure.QAtom x
    (Curry.QCons car cdr) ->
      Closure.QCons (convertQQ env fvs car) (convertQQ env fvs cdr)
    (Curry.UQ e) -> Closure.UQ $ convert env fvs e
    (Curry.UQS e) -> Closure.UQS $ convert env fvs e

convert :: [Name] -> [Name] -> Curry.Expr -> Closure.Expr
convert env fvs expr =
  case expr of
    (Curry.Q x) -> Closure.Q x
    (Curry.QQ x) -> Closure.QQ $ convertQQ env fvs x
    (Curry.BinOp op e1 e2) ->
      Closure.BinOp op (convert env fvs e1) (convert env fvs e2)
    (Curry.Var x) ->
      if x `elem` fvs
        then Closure.Ref $ fromJust $ elemIndex x fvs
        else Closure.Var x
    (Lam x e) -> Closure x (convert env fvs' e) fvs'
      where fvs' = sort $ nub $ free e `without` (x : env) ++ fvs
    (Curry.Let x e1 e2) ->
      Closure.Let x (convert env' fvs' e1) (convert env' fvs' e2)
      where env' = x : env
            fvs' = fvs `without` [x]
    (Curry.If cond tr fl) ->
      Closure.If
        (convert env fvs cond)
        (convert env fvs tr)
        (convert env fvs fl)
    (Curry.App e1 e2) -> Closure.App (convert env fvs e1) (convert env fvs e2)
    (Curry.Fix e) -> Closure.Fix $ convert env fvs e
    (Curry.Case e clauses) -> Closure.Case (convert env fvs e) clauses'
      where (bindings, bodies) = unzip clauses
            (names, types) = unzip bindings
            types' = map (convertQQ env fvs) types
            bindings' = zip names types'
            bodies' = map (convert env fvs) bodies
            clauses' = zip bindings' bodies'
