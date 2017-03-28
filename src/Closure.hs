module Closure where

import Data.List

import Core
import Environment
import Pretty ()
import Type

type Name = String

isPolymorphic :: Environment -> Top Typed -> Bool
isPolymorphic _ (Define (_, TypeVariable _) _ _) = True
isPolymorphic _ (Define _ [(_, TypeVariable _)] _) = True
isPolymorphic _ Define {} = False
isPolymorphic _ (Declare _ _) = False
isPolymorphic env (Command expr) =
  case expr of
    Quote _ -> False
    Quasiquote sexp -> isPolymorphic' sexp
      where isPolymorphic' :: Quasisexp Typed -> Bool
            isPolymorphic' (Quasiatom _) = False
            isPolymorphic' (Quasicons car cdr) =
              isPolymorphic' car && isPolymorphic' cdr
            isPolymorphic' (Unquote x) = isPolymorphic env (Command x)
            isPolymorphic' (UnquoteSplicing x) = isPolymorphic env (Command x)
    BinOp _ e1 e2 ->
      isPolymorphic env (Command e1) && isPolymorphic env (Command e2)
    Variable (_, TypeVariable _) _ -> True
    Variable (_, _) _ -> False
    Lambda _ (TypeVariable _) _ _ -> True
    Lambda [(_, TypeVariable _)] _ _ _ -> True
    Lambda {} -> False
    Let [((_, TypeVariable _), _)] _ -> True
    Let _ _ -> False
    If cond tr fl ->
      isPolymorphic env (Command cond) &&
      isPolymorphic env (Command tr) && isPolymorphic env (Command fl)
    Call f args ->
      isPolymorphic env (Command f) &&
      foldl (\e x -> isPolymorphic env (Command x) && e) True args
    Case e clauses ->
      isPolymorphic env (Command e) &&
      foldl (\acc x -> isPolymorphic env (Command x) && acc) True bodies
      where (_, bodies) = unzip clauses

convert' :: [Name] -> [Name] -> Quasisexp Typed -> Quasisexp Typed
convert' env fvs expr =
  case expr of
    (Quasiatom x) -> Quasiatom x
    (Quasicons car cdr) ->
      Quasicons (convert' env fvs car) (convert' env fvs cdr)
    (Unquote e) -> Unquote $ convert env fvs e
    (UnquoteSplicing e) -> UnquoteSplicing $ convert env fvs e

convert :: [Name] -> [Name] -> Expression Typed -> Expression Typed
convert env fvs expr =
  case expr of
    (Quote x) -> Quote x
    (Quasiquote x) -> Quasiquote $ convert' env fvs x
    (BinOp op e1 e2) -> BinOp op (convert env fvs e1) (convert env fvs e2)
    (Variable x r) ->
      let (name, _) = x
      in case elemIndex name fvs of
           Nothing -> Variable x Nothing
           Just idx -> Variable x (Just idx)
    (Lambda x t _ e) -> Lambda x t fvs' (convert env fvs' e)
      where (name, _) = head x
            fvs' = sort $ nub $ free e `without` (name : env) ++ fvs
    (Let b e2) -> Let [(x, convert env' fvs' e1)] (convert env' fvs' e2)
      where (x, e1) = head b
            (name, _) = x
            env' = name : env
            fvs' = fvs `without` [name]
    (If cond tr fl) ->
      If (convert env fvs cond) (convert env fvs tr) (convert env fvs fl)
    (Call e1 e2) -> Call (convert env fvs e1) (map (convert env fvs) e2)
    (Case e clauses) -> Case (convert env fvs e) clauses'
      where (bindings, bodies) = unzip clauses
            bodies' = map (convert env fvs) bodies
            clauses' = zip bindings bodies'

without
  :: Eq a
  => [a] -> [a] -> [a]
without = foldr (filter . (/=))

free :: Expression Typed -> [Name]
free (Quote _) = []
free (Quasiquote x) = vars x
free (BinOp _ e1 e2) = free e1 ++ free e2
free (Variable (x, _) _) = [x]
free (Lambda x _ _ e) = free e `without` [fst (head x)]
free (Let binds e2) = concatMap free es ++ (free e2 `without` names)
  where
    (xs, es) = unzip binds
    (names, _) = unzip xs
free (If cond tr fl) = free cond ++ free tr ++ free fl
free (Call e1 e2) = free e1 ++ concatMap free e2
free (Case x clauses) = (free x ++ bodies') `without` bindings'
  where
    (bindings, bodies) = unzip clauses
    (bindings', _) = unzip bindings
    bodies' = concatMap free bodies

vars :: Quasisexp Typed -> [Name]
vars (Quasiatom _) = []
vars (Quasicons car cdr) = vars car ++ vars cdr
vars (Unquote e) = free e
vars (UnquoteSplicing e) = free e
