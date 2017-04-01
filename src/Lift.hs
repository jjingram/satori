module Lift
  ( lambdaLiftProgram
  ) where

import Data.List (sort, nub)

import Control.Monad.State
import Control.Monad.Writer

import Syntax
import Type

without
  :: Eq a
  => [a] -> [a] -> [a]
without = foldr (filter . (/=))

free :: Expression Typed -> [Typed]
free (Quote _) = []
free (Quasiquote x) = free' x
free (BinOp _ e1 e2) = free e1 ++ free e2
free (Variable x) = [x]
free (Lambda x e) = free e `without` x
free (Let binds e2) = concatMap free es ++ free e2 `without` xs
  where
    (xs, es) = unzip binds
free (If cond tr fl) = free cond ++ free tr ++ free fl
free (Call e1 e2) = free e1 ++ concatMap free e2
free (Case x clauses) = (free x ++ bodies') `without` names
  where
    (bindings, bodies) = unzip clauses
    (names, _) = unzip bindings
    bodies' = concatMap free bodies
free (Fix x) = free x

free' :: Quasisexp Typed -> [Typed]
free' (Quasiatom _) = []
free' (Quasicons car cdr) = free' car ++ free' cdr
free' (Unquote e) = free e
free' (UnquoteSplicing e) = free e

convert' :: [Typed] -> [Typed] -> Quasisexp Typed -> Quasisexp Typed
convert' env fvs expr =
  case expr of
    (Quasiatom x) -> Quasiatom x
    (Quasicons car cdr) ->
      Quasicons (convert' env fvs car) (convert' env fvs cdr)
    (Unquote e) -> Unquote $ convert env fvs e
    (UnquoteSplicing e) -> UnquoteSplicing $ convert env fvs e

convert :: [Typed] -> [Typed] -> Expression Typed -> Expression Typed
convert env fvs expr =
  case expr of
    (Quote x) -> Quote x
    (Quasiquote x) -> Quasiquote $ convert' env fvs x
    (BinOp op e1 e2) -> BinOp op (convert env fvs e1) (convert env fvs e2)
    (Variable x) -> Variable x
    (Lambda x e) -> Lambda (x ++ fvs') (convert env fvs' e)
      where x' = head x
            fvs' = sort $ nub $ (free e ++ fvs) `without` (x' : env)
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

type Lift a = WriterT [Top Typed] (State Word) a

fresh :: Lift String
fresh = do
  count <- lift get
  lift $ put (count + 1)
  return $ show count

qqLambdaLift :: Quasisexp Typed -> Lift (Quasisexp Typed)
qqLambdaLift x@(Quasiatom _) = return x
qqLambdaLift (Quasicons car cdr) = do
  car' <- qqLambdaLift car
  cdr' <- qqLambdaLift cdr
  return $ Quasicons car' cdr'
qqLambdaLift (Unquote e) = do
  e' <- lambdaLift e
  return $ Unquote e'
qqLambdaLift (UnquoteSplicing e) = do
  e' <- lambdaLift e
  return $ UnquoteSplicing e'

lambdaLift :: Expression Typed -> Lift (Expression Typed)
lambdaLift (Quote x) = return $ Quote x
lambdaLift (Quasiquote x) = do
  x' <- qqLambdaLift x
  return $ Quasiquote x'
lambdaLift (BinOp op e1 e2) = do
  e1' <- lambdaLift e1
  e2' <- lambdaLift e2
  return $ BinOp op e1' e2'
lambdaLift (Variable x) = return $ Variable x
lambdaLift (Lambda x e) = do
  let (_, ty) = head x
  name <- fresh
  e' <- lambdaLift e
  let ty' = TypeArrow ty (typeOf e')
  let def = Define (name, ty') x e'
  tell [def]
  return $ Variable (name, ty')
lambdaLift (Let b e2) = do
  let (x, e1) = head b
  e1' <- lambdaLift e1
  e2' <- lambdaLift e2
  return $ Let [(x, e1')] e2'
lambdaLift (If cond tr fl) = do
  cond' <- lambdaLift cond
  tr' <- lambdaLift tr
  fl' <- lambdaLift fl
  return $ If cond' tr' fl'
lambdaLift (Call e1 e2) = do
  e1' <- lambdaLift e1
  e2' <- mapM lambdaLift e2
  return $ Call e1' e2'
lambdaLift (Case e clauses) = do
  e' <- lambdaLift e
  let (bindings, bodies) = unzip clauses
  bodies' <- mapM lambdaLift bodies
  let clauses' = zip bindings bodies'
  return $ Case e' clauses'
lambdaLift (Fix e) = do
  e' <- lambdaLift e
  return $ Fix e'

lambdaLiftTop :: Word -> [Typed] -> Top Typed -> (Program Typed, Word)
lambdaLiftTop count globals top =
  case top of
    (Define _ [] body) -> (defs, count')
      where ((_, defs), count') =
              flip runState count . runWriterT . lambdaLift $
              convert globals [] body
    (Command body) -> (defs ++ [Command body'], count')
      where ((body', defs), count') =
              flip runState count . runWriterT . lambdaLift $
              convert globals [] body
    _ -> ([top], count)

lambdaLiftProgram :: Word -> [Typed] -> Program Typed -> (Program Typed, Word)
lambdaLiftProgram count _ [] = ([], count)
lambdaLiftProgram count globals (top@Define {}:rest) = (rest', count'')
  where
    (rest', count'') = lambdaLiftProgram count' globals rest
    (_, count') = lambdaLiftTop count globals top
lambdaLiftProgram count globals (top@Command {}:rest) = (cmd ++ rest', count'')
  where
    (rest', count'') = lambdaLiftProgram count' globals rest
    (cmd, count') = lambdaLiftTop count globals top
lambdaLiftProgram count globals (top@Declare {}:rest) = (decl ++ rest', count'')
  where
    (rest', count'') = lambdaLiftProgram count' globals rest
    (decl, count') = lambdaLiftTop count globals top
