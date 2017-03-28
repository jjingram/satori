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

import Core
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

runInfer
  :: Environment
  -> Infer (Type, [Constraint], Core.Expression Typed)
  -> Either TypeError (Type, [Constraint], Core.Expression Typed)
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

inferExpr :: Environment -> Syntax.Expression Name -> Either TypeError Scheme
inferExpr env ex =
  case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs, _) ->
      case runSolve cs of
        Left err -> Left err
        Right subst -> Right $ closeOver $ apply subst ty

constraintsExpr
  :: Environment
  -> Syntax.Expression Name
  -> Either TypeError ([Constraint], Subst, Type, Scheme, Core.Expression Typed)
constraintsExpr env ex =
  case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs, ex') ->
      case runSolve cs of
        Left err -> Left err
        Right subst -> Right (cs, subst, ty, sc, ex')
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

ops :: Map.Map Op (Name, Type)
ops =
  Map.fromList
    [ (Add, ("add", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (Mul, ("mul", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (Sub, ("sub", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (SDiv, ("sdiv", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (SRem, ("srem", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (ILT, ("ilt", i64 `TypeArrow` (i64 `TypeArrow` i1)))
    ]

infer :: Syntax.Expression Name
      -> Infer (Type, [Constraint], Core.Expression Typed)
infer expr =
  case expr of
    Syntax.Quote sexp -> return (inferQuote sexp, [], Core.Quote sexp)
    Syntax.Quasiquote sexp -> do
      (t, c, sexp') <- inferQuasiquote sexp
      return (t, c, Core.Quasiquote sexp')
    Syntax.BinOp op e1 e2 -> do
      (t1, c1, e1') <- infer e1
      (t2, c2, e2') <- infer e2
      tv <- fresh
      let u1 = t1 `TypeArrow` (t2 `TypeArrow` tv)
          u2 = snd $ ops Map.! op
      return (tv, c1 ++ c2 ++ [(u1, u2)], Core.BinOp op e1' e2')
    Syntax.Variable x -> do
      t <- lookupEnvironment x
      return (t, [], Core.Variable (x, t) Nothing)
    Syntax.Lambda x e -> do
      tv <- fresh
      let name = head x
      (t, c, e') <- inEnvironment (name, Forall [] tv) (infer e)
      return (tv `TypeArrow` t, c, Core.Lambda [(name, tv)] t [] e')
    Syntax.Call e1 e2 -> do
      let e2' = head e2
      (t1, c1, e1') <- infer e1
      (t2, c2, e2'') <- infer e2'
      tv <- fresh
      return (tv, c1 ++ c2 ++ [(t1, t2 `TypeArrow` tv)], Core.Call e1' [e2''])
    Syntax.Let b e2 -> do
      env <- ask
      let (x, e1) = head b
      (t1, c1, e1') <- infer e1
      case runSolve c1 of
        Left err -> throwError err
        Right sub -> do
          let sc = generalize (apply sub env) (apply sub t1)
          (t2, c2, e2') <- inEnvironment (x, sc) $ local (apply sub) (infer e2)
          return (t2, c1 ++ c2, Core.Let [((x, t1), e1')] e2')
    Syntax.If cond tr fl -> do
      (t1, c1, cond') <- infer cond
      (t2, c2, tr') <- infer tr
      (t3, c3, fl') <- infer fl
      return
        (TypeSum t2 t3, c1 ++ c2 ++ c3 ++ [(t1, i1)], Core.If cond' tr' fl')
    Syntax.Case e clauses -> do
      let (bindings, bodies) = unzip clauses
      let (names, patterns) = unzip bindings
      let ts1 = map inferQuote patterns
      -- TODO: write `memberOf` function to check that the pattern types are part
      -- of the expression's sum type.
      (_, c, e') <- infer e
      tvs <- replicateM (length names) fresh
      xs <-
        mapM
          (\(x, body, tv) -> inEnvironment (x, Forall [] tv) (infer body))
          (zip3 names bodies tvs)
      let (ts2, cs2, bodies') = unzip3 xs
      let cs3 = zip ts1 ts2
      let clauses' = zip bindings bodies'
      let t2 = foldr1 TypeSum ts2
      return (t2, c ++ concat cs2 ++ cs3, Core.Case e' clauses')

inferQuote :: Sexp -> Type
inferQuote (Atom Nil) = unit
inferQuote (Atom (Integer _)) = i64
inferQuote (Atom (Symbol s)) = TypeSymbol s
inferQuote (Cons car cdr) = TypeProduct (inferQuote car) (inferQuote cdr)

inferQuasiquote :: Syntax.Quasisexp Name
                -> Infer (Type, [Constraint], Core.Quasisexp Typed)
inferQuasiquote (Syntax.Quasiatom Nil) = return (unit, [], Core.Quasiatom Nil)
inferQuasiquote (Syntax.Quasiatom (Integer n)) =
  return (i64, [], Core.Quasiatom (Integer n))
inferQuasiquote (Syntax.Quasiatom (Symbol s)) =
  return (TypeSymbol s, [], Core.Quasiatom (Symbol s))
inferQuasiquote (Syntax.Unquote x) = do
  (t, c, x') <- infer x
  return (t, c, Core.Unquote x')
inferQuasiquote (Syntax.UnquoteSplicing x) = do
  (t, c, x') <- infer x
  return (t, c, Core.UnquoteSplicing x')
inferQuasiquote (Syntax.Quasicons car cdr) = do
  (t1, c1, car') <- inferQuasiquote car
  (t2, c2, cdr') <- inferQuasiquote cdr
  return (TypeProduct t1 t2, c1 ++ c2, Core.Quasicons car' cdr')

inferTop :: Environment
         -> [(Name, Syntax.Expression Name)]
         -> Either TypeError Environment
inferTop env [] = Right env
inferTop env ((x, ex):xs) =
  case inferExpr env ex of
    Left err -> Left err
    Right ty -> inferTop (extend env (x, ty)) xs

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
