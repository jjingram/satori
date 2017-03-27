{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infer
  ( Constraint
  , TypeError(..)
  , Subst(..)
  , inferTop
  , constraintsExpr
  , ops
  ) where

import Curry
import Environment
import Syntax
import Type

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Infer a = (ReaderT Environment (StateT InferState (Except TypeError)) a)

newtype InferState = InferState
  { count :: Int
  }

initInfer :: InferState
initInfer = InferState {count = 0}

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

type Solve a = ExceptT TypeError Identity a

newtype Subst =
  Subst (Map.Map TypeVariable Type)
  deriving (Eq, Ord, Show, Monoid)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TypeVariable

-- TODO: implement for TypeProduct and TypeSum
instance Substitutable Type where
  apply _ (TypeSymbol a) = TypeSymbol a
  apply (Subst s) t@(TypeVariable a) = Map.findWithDefault t a s
  apply s (t1 `TypeArrow` t2) = apply s t1 `TypeArrow` apply s t2
  ftv TypeSymbol {} = Set.empty
  ftv (TypeVariable a) = Set.singleton a
  ftv (t1 `TypeArrow` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply (Subst s) (Forall as t) = Forall as $ apply s' t
    where
      s' = Subst $ foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
  apply s (t1, t2) = (apply s t1, apply s t2)
  ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a =>
         Substitutable [a] where
  apply = map . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable Environment where
  apply s (TypeEnvironment env) = TypeEnvironment $ Map.map (apply s) env
  ftv (TypeEnvironment env) = ftv $ Map.elems env

data TypeError
  = UnificationFail Type
                    Type
  | InfiniteType TypeVariable
                 Type
  | UnboundVariable String
  | Ambigious [Constraint]
  | UnificationMismatch [Type]
                        [Type]
  | MalformedPattern QSexp

runInfer
  :: Environment
  -> Infer (Type, [Constraint])
  -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

inferExpr :: Environment -> Expr -> Either TypeError Scheme
inferExpr env ex =
  case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs) ->
      case runSolve cs of
        Left err -> Left err
        Right subst -> Right $ closeOver $ apply subst ty

constraintsExpr :: Environment
                -> Expr
                -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsExpr env ex =
  case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs) ->
      case runSolve cs of
        Left err -> Left err
        Right subst -> Right (cs, subst, ty, sc)
          where sc = closeOver $ apply subst ty

closeOver :: Type -> Scheme
closeOver = normalize . generalize Environment.empty

inEnvironment :: (Name, Scheme) -> Infer a -> Infer a
inEnvironment (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

lookupEnvironment :: Name -> Infer Type
lookupEnvironment x = do
  (TypeEnvironment env) <- ask
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s -> instantiate s

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s {count = count s + 1}
  return $ TypeVariable $ TV (letters !! count s)

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ apply s t

generalize :: Environment -> Type -> Scheme
generalize env t = Forall as t
  where
    as = Set.toList $ ftv t `Set.difference` ftv env

ops :: Map.Map Syntax.Op (Name, Type)
ops =
  Map.fromList
    [ (Add, ("add", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (Mul, ("mul", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (Sub, ("sub", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (SDiv, ("sdiv", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (SRem, ("srem", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (ILT, ("ilt", i64 `TypeArrow` (i64 `TypeArrow` i1)))
    ]

infer :: Expr -> Infer (Type, [Constraint])
infer expr =
  case expr of
    Q sexp -> return (qType sexp, [])
    QQ sexp -> return (qqType sexp, [])
    Curry.BinOp op e1 e2 -> do
      (t1, c1) <- infer e1
      (t2, c2) <- infer e2
      tv <- fresh
      let u1 = t1 `TypeArrow` (t2 `TypeArrow` tv)
          u2 = snd $ ops Map.! op
      return (tv, c1 ++ c2 ++ [(u1, u2)])
    Var x -> do
      t <- lookupEnvironment x
      return (t, [])
    Lam x e -> do
      tv <- fresh
      (t, c) <- inEnvironment (x, Forall [] tv) (infer e)
      return (tv `TypeArrow` t, c)
    App e1 e2 -> do
      (t1, c1) <- infer e1
      (t2, c2) <- infer e2
      tv <- fresh
      return (tv, c1 ++ c2 ++ [(t1, t2 `TypeArrow` tv)])
    Curry.Let x e1 e2 -> do
      env <- ask
      (t1, c1) <- infer e1
      case runSolve c1 of
        Left err -> throwError err
        Right sub -> do
          let sc = generalize (apply sub env) (apply sub t1)
          (t2, c2) <- inEnvironment (x, sc) $ local (apply sub) (infer e2)
          return (t2, c1 ++ c2)
    Fix e1 -> do
      (t1, c1) <- infer e1
      tv <- fresh
      return (tv, c1 ++ [(tv `TypeArrow` tv, t1)])
    Curry.If cond tr fl -> do
      (t1, c1) <- infer cond
      (t2, c2) <- infer tr
      (t3, c3) <- infer fl
      return (TypeSum t2 t3, c1 ++ c2 ++ c3 ++ [(t1, i1)])
    Curry.Case e clauses -> do
      let (bindings, bodies) = unzip clauses
      let (names, patterns) = unzip bindings
      -- TODO: write `memberOf` function to check that the clause types are part
      -- of the expression's sum type.
      (_, c1) <- infer e
      patterns' <- mapM qqType patterns
      tvs <- replicateM (length names) fresh
      let (ts1, cs1) = unzip patterns'
      let cs1' = zip tvs ts1
      xs <-
        mapM
          (\(x, e', tv) -> inEnvironment (x, Forall [] tv) (infer e'))
          (zip3 names bodies tvs)
      let (ts2, cs2) = unzip xs
      let t2 = foldr1 TypeSum ts2
      let cs2' = concat cs2
      return (t2, c1 ++ concat cs1 ++ cs1' ++ cs2')

qType :: Sexp -> Type
qType (Atom Nil) = unit
qType (Atom (Integer _)) = i64
qType (Atom (Symbol s)) = TypeSymbol s
qType (Cons car cdr) = TypeProduct (qType car) (qType cdr)

qqType :: QSexp -> Infer (Type, [Constraint])
qqType (QAtom Nil) = return (unit, [])
qqType (QAtom (Integer _)) = return (i64, [])
qqType (UQ _) = do
  tv <- fresh
  return (tv, [])
qqType x@(UQS _) = do
  tv <- fresh
  return (tv, [])
qqType (QAtom (Symbol s)) = return (TypeSymbol s, [])
qqType (QCons (UQ _) cdr) = do
  tv <- fresh
  (t1, c1) <- qqType cdr
  return $ (t1, c1 ++ [(tv, t1)])
qqType (QCons (UQS _) cdr) = do
  tv <- fresh
  (t1, c1) <- qqType cdr
  return $ (t1, c1 ++ [(tv, t1)])
qqType (QCons car cdr) = do
  (t1, c1) <- qqType car
  (t2, c2) <- qqType cdr
  return $ (TypeProduct t1 t2, c1 ++ c2)

inferTop :: Environment -> [(String, Expr)] -> Either TypeError Environment
inferTop env [] = Right env
inferTop env ((name, ex):xs) =
  case inferExpr env ex of
    Left err -> Left err
    Right ty -> inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)
    fv (TypeVariable a) = [a]
    fv (TypeArrow a b) = fv a ++ fv b
    fv (TypeSymbol _) = []
    normtype (TypeArrow a b) = TypeArrow (normtype a) (normtype b)
    normtype (TypeSymbol a) = TypeSymbol a
    normtype (TypeVariable a) =
      case Prelude.lookup a ord of
        Just x -> TypeVariable x
        Nothing -> error "type variable not in signature"

emptySubst :: Subst
emptySubst = mempty

compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) =
  Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver st
  where
    st = (emptySubst, cs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1:ts1) (t2:ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
  return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2
  | t1 == t2 = return emptySubst
unifies (TypeVariable v) t = v `bind` t
unifies t (TypeVariable v) = v `bind` t
unifies (TypeArrow t1 t2) (TypeArrow t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2):cs0) -> do
      su1 <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

bind :: TypeVariable -> Type -> Solve Subst
bind a t
  | t == TypeVariable a = return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return (Subst $ Map.singleton a t)

occursCheck
  :: Substitutable a
  => TypeVariable -> a -> Bool
occursCheck a t = a `Set.member` ftv t
