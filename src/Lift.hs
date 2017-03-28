module Lift where

import Control.Monad.State
import Control.Monad.Writer

import Closure
import Syntax

type Lift a = WriterT [Top Core] (State Integer) a

fresh :: Lift Name
fresh = do
  count <- lift get
  lift $ put (count + 1)
  return $ show count

qqLambdaLift :: Quasisexp Core -> Lift (Quasisexp Core)
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

lambdaLift :: Expression Core -> Lift (Expression Core)
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
  name <- fresh
  e' <- lambdaLift e
  let Closure (name', _, pos) = head x
  let def = Define (Name name) [Id (name', pos)] e'
  tell [def]
  return $ Lambda x e'
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
  let (names, types) = unzip bindings
  types' <- mapM qqLambdaLift types
  bodies' <- mapM lambdaLift bodies
  let bindings' = zip names types'
  let clauses' = zip bindings' bodies'
  return $ Case e' clauses'

lambdaLiftTop :: [Name] -> Top Id -> Program Core
lambdaLiftTop globals (Define name [] body) =
  defs ++ [Define (coreId name) [] body']
  where
    (body', defs) =
      flip evalState 0 . runWriterT . lambdaLift $ convert globals [] body
lambdaLiftTop _ _ = []

lambdaLiftProgram :: [Name] -> Program Id -> Program Core
lambdaLiftProgram _ [] = []
lambdaLiftProgram globals (top@Define {}:rest) =
  def ++ lambdaLiftProgram (name : globals) rest
  where
    def@(Define (Name name) _ _:_) = lambdaLiftTop globals top
lambdaLiftProgram globals (_:rest) = lambdaLiftProgram globals rest
