module Lift where

import Control.Monad.State
import Control.Monad.Writer

import Closure
import Curry
import Syntax

type Program = [Lift.Top]

type Env = [Name]

data Top
  = Def Name
        [Name]
        Lift.Expr
  | Decl Name
         [Name]
  | Cmd Lift.Expr
  deriving (Eq, Ord, Show)

data Expr
  = Q Sexp
  | QQ Lift.QSexp
  | BinOp Op
          Lift.Expr
          Lift.Expr
  | Var Name
  | Ref Name
  | Let Name
        Lift.Expr
        Lift.Expr
  | If Lift.Expr
       Lift.Expr
       Lift.Expr
  | App Lift.Expr
        Lift.Expr
  | Fix Lift.Expr
  | Case Lift.Expr
         [((Name, Lift.QSexp), Lift.Expr)]
  | Closure Name
            Lift.Env
  deriving (Eq, Ord, Show)

data QSexp
  = QAtom Atom
  | QCons Lift.QSexp
          Lift.QSexp
  | UQ Lift.Expr
  | UQS Lift.Expr
  deriving (Eq, Ord, Show)

type Lift a = WriterT [Lift.Top] (State Integer) a

fresh :: Lift Name
fresh = do
  count <- lift get
  lift $ put (count + 1)
  return $ show count

qqLambdaLift :: Closure.QSexp -> Lift Lift.QSexp
qqLambdaLift (Closure.QAtom x) = return $ Lift.QAtom x
qqLambdaLift (Closure.QCons car cdr) = do
  car' <- qqLambdaLift car
  cdr' <- qqLambdaLift cdr
  return $ Lift.QCons car' cdr'
qqLambdaLift (Closure.UQ e) = do
  e' <- lambdaLift e
  return $ Lift.UQ e'
qqLambdaLift (Closure.UQS e) = do
  e' <- lambdaLift e
  return $ Lift.UQS e'

lambdaLift :: Closure.Expr -> Lift Lift.Expr
lambdaLift (Closure.Q x) = return $ Lift.Q x
lambdaLift (Closure.QQ x) = do
  x' <- qqLambdaLift x
  return $ Lift.QQ x'
lambdaLift (Closure.BinOp op e1 e2) = do
  e1' <- lambdaLift e1
  e2' <- lambdaLift e2
  return $ Lift.BinOp op e1' e2'
lambdaLift (Closure.Ref x) = return $ Lift.Ref x
lambdaLift (Closure.Var x) = return $ Lift.Var x
lambdaLift (Closure.Let x e1 e2) = do
  e1' <- lambdaLift e1
  e2' <- lambdaLift e2
  return $ Lift.Let x e1' e2'
lambdaLift (Closure.If t tr fl) = do
  t' <- lambdaLift t
  tr' <- lambdaLift tr
  fl' <- lambdaLift fl
  return $ Lift.If t' tr' fl'
lambdaLift (Closure.App e1 e2) = do
  e1' <- lambdaLift e1
  e2' <- lambdaLift e2
  return $ Lift.App e1' e2'
lambdaLift (Closure.Fix e) = do
  e' <- lambdaLift e
  return $ Lift.Fix e'
lambdaLift (Closure.Case e clauses) = do
  let (bindings, bodies) = unzip clauses
  let (names, types) = unzip bindings
  types' <- mapM qqLambdaLift types
  bodies' <- mapM lambdaLift bodies
  e' <- lambdaLift e
  let bindings' = zip names types'
  let clauses' = zip bindings' bodies'
  return $ Lift.Case e' clauses'
lambdaLift (Closure.Closure x e env) = do
  name <- fresh
  e' <- lambdaLift e
  let def = Lift.Def name [x] e'
  tell [def]
  return $ Lift.Closure x env

lambdaLiftTop :: [Name] -> Curry.Top -> Lift.Program
lambdaLiftTop globals (Curry.Def name body) = defs ++ [Lift.Def name [] body']
  where
    (body', defs) =
      flip evalState 0 . runWriterT . lambdaLift $ convert globals [] body
lambdaLiftTop _ _ = []

lambdaLiftProgram :: [Name] -> Curry.Program -> Lift.Program
lambdaLiftProgram _ [] = []
lambdaLiftProgram globals (top@(Curry.Def _ _):rest) =
  def ++ lambdaLiftProgram (name : globals) rest
  where
    def@(Lift.Def name _ _:_) = lambdaLiftTop globals top
lambdaLiftProgram globals (_:rest) = lambdaLiftProgram globals rest
