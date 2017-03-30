module Lift where

import Control.Monad.State
import Control.Monad.Writer

import Closure
import Core

type Lift a = WriterT [Top Typed] (State Word) a

fresh :: Lift Word
fresh = do
  count <- lift get
  lift $ put (count + 1)
  return count

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
lambdaLift (Variable x r) = return $ Variable x r
lambdaLift (Lambda _ x t f e) = do
  name <- fresh
  e' <- lambdaLift e
  let def = Define (name, t) (x ++ f) e'
  tell [def]
  return $ Lambda name x t f e'
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

lambdaLiftTop :: Word -> Free -> Top Typed -> (Program Typed, Word)
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

lambdaLiftProgram :: Word -> Free -> Program Typed -> (Program Typed, Word)
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
