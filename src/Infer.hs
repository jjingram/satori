{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infer
  ( Constraint
  , TypeError(..)
  , Subst(..)
  , inferTop
  , constraintsTop
  , binops
  , inferQuote
  ) where

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
  apply s (t1 `TypeProduct` t2) = apply s t1 `TypeProduct` apply s t2
  apply s (t1 `TypeSum` t2) = apply s t1 `TypeSum` apply s t2
  ftv TypeSymbol {} = Set.empty
  ftv (TypeVariable a) = Set.singleton a
  ftv (t1 `TypeArrow` t2) = ftv t1 `Set.union` ftv t2
  ftv (t1 `TypeProduct` t2) = ftv t1 `Set.union` ftv t2
  ftv (t1 `TypeSum` t2) = ftv t1 `Set.union` ftv t2

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
  -> Infer (Type, [Constraint], Expression Typed)
  -> Either TypeError (Type, [Constraint], Expression Typed)
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

inferExpr :: Environment -> Expression Name -> Either TypeError Scheme
inferExpr env ex =
  case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs, _) ->
      case runSolve cs of
        Left err -> Left err
        Right subst -> Right $ closeOver $ apply subst ty

substituteType :: Subst -> Expression Typed -> Expression Typed
substituteType sub expr =
  case expr of
    Quote sexp -> Quote sexp
    Quasiquote sexp -> Quasiquote $ substituteType' sexp
      where substituteType' :: Quasisexp Typed -> Quasisexp Typed
            substituteType' sexp' =
              case sexp' of
                Quasiatom x -> Quasiatom x
                Quasicons car cdr ->
                  Quasicons (substituteType' car) (substituteType' cdr)
                Unquote x -> Unquote $ substituteType sub x
                UnquoteSplicing x -> UnquoteSplicing $ substituteType sub x
    BinOp op e1 e2 -> BinOp op (substituteType sub e1) (substituteType sub e2)
    Variable (name, ty) -> Variable (name, ty')
      where ty' = apply sub ty
    Lambda x e -> Lambda x' (substituteType sub e)
      where (name, ty) = head x
            ty' = apply sub ty
            x' = [(name, ty')]
    Let x e -> Let x' (substituteType sub e)
      where ((name, ty), e1) = head x
            ty' = apply sub ty
            x' = [((name, ty'), substituteType sub e1)]
    If cond tr fl ->
      If
        (substituteType sub cond)
        (substituteType sub tr)
        (substituteType sub fl)
    Call f args -> Call (substituteType sub f) (map (substituteType sub) args)
    Fix x e -> Fix x' (substituteType sub e)
      where (name, ty) = x
            ty' = apply sub ty
            x' = (name, ty')

constraintsTop :: Environment -> Program Name -> [Either TypeError (Top Typed)]
constraintsTop _ [] = []
constraintsTop env (Define name body:rest) =
  case constraintsExpr env body of
    Left err -> Left err : constraintsTop env rest
    Right (_, _, _, Forall _ t, body') ->
      Right (Define (name, t) body') : constraintsTop env rest
constraintsTop env (Declare {}:rest) = constraintsTop env rest
constraintsTop env (Command expr:rest) =
  case constraintsExpr env expr of
    Left err -> Left err : constraintsTop env rest
    Right (_, _, _, _, expr') -> Right (Command expr') : constraintsTop env rest

constraintsExpr
  :: Environment
  -> Expression Name
  -> Either TypeError ([Constraint], Subst, Type, Scheme, Expression Typed)
constraintsExpr env ex =
  case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs, ex') ->
      case runSolve cs of
        Left err -> Left err
        Right subst -> Right (cs, subst, ty, sc, ex'')
          where sc = closeOver $ apply subst ty
                ex'' = substituteType subst ex'

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

infer :: Expression Name -> Infer (Type, [Constraint], Expression Typed)
infer expr =
  case expr of
    Quote sexp@(Atom Nil) -> return (inferQuote sexp, [], Quote (Atom Nil))
    Quote sexp@(Atom (Integer n)) ->
      return (inferQuote sexp, [], Quote (Atom (Integer n)))
    Quote sexp@(Atom (Symbol s)) ->
      return (inferQuote sexp, [], Quote (Atom (Symbol s)))
    Quote sexp@(Cons car cdr) ->
      return (inferQuote sexp, [], Quote (Cons car cdr))
    Quasiquote sexp -> do
      (t, c, sexp') <- inferQuasiquote sexp
      return (t, c, Quasiquote sexp')
    BinOp op e1 e2 -> do
      (t1, c1, e1') <- infer e1
      (t2, c2, e2') <- infer e2
      tv <- fresh
      let u1 = t1 `TypeArrow` (t2 `TypeArrow` tv)
          u2 = snd $ Syntax.binops Map.! op
      return (tv, c1 ++ c2 ++ [(u1, u2)], BinOp op e1' e2')
    Variable x -> do
      t <- lookupEnvironment x
      return (t, [], Variable (x, t))
    Lambda x e -> do
      tv <- fresh
      let name = head x
      (t, c, e') <- inEnvironment (name, Forall [] tv) (infer e)
      return (tv `TypeArrow` t, c, Lambda [(name, tv)] e')
    Call e1 e2 -> do
      let e2' = head e2
      (t1, c1, e1') <- infer e1
      (t2, c2, e2'') <- infer e2'
      tv <- fresh
      return (tv, c1 ++ c2 ++ [(t1, t2 `TypeArrow` tv)], Call e1' [e2''])
    Let b e2 -> do
      env <- ask
      let (x, e1) = head b
      (t1, c1, e1') <- infer e1
      case runSolve c1 of
        Left err -> throwError err
        Right sub -> do
          let sc = generalize (apply sub env) (apply sub t1)
          (t2, c2, e2') <- inEnvironment (x, sc) $ local (apply sub) (infer e2)
          return (t2, c1 ++ c2, Let [((x, t1), e1')] e2')
    If cond tr fl -> do
      (t1, c1, cond') <- infer cond
      (t2, c2, tr') <- infer tr
      (t3, c3, fl') <- infer fl
      return (t2, c1 ++ c2 ++ c3 ++ [(t1, i1), (t2, t3)], If cond' tr' fl')
    Fix name e -> do
      tv <- fresh
      (ty, c, e') <- inEnvironment (name, Forall [] tv) (infer e)
      return (tv, c ++ [(tv, ty)], Fix (name, tv) e')

inferQuote :: Sexp -> Type
inferQuote (Atom Nil) = nil
inferQuote (Atom (Integer _)) = i64
inferQuote (Atom (Symbol s)) = TypeSymbol s
inferQuote (Cons car cdr) = TypeProduct (inferQuote car) (inferQuote cdr)

inferQuasiquote :: Quasisexp Name -> Infer (Type, [Constraint], Quasisexp Typed)
inferQuasiquote (Quasiatom Nil) = return (nil, [], Quasiatom Nil)
inferQuasiquote (Quasiatom (Integer n)) =
  return (i64, [], Quasiatom (Integer n))
inferQuasiquote (Quasiatom (Symbol s)) =
  return (TypeSymbol s, [], Quasiatom (Symbol s))
inferQuasiquote (Unquote x) = do
  (t, c, x') <- infer x
  return (t, c, Unquote x')
inferQuasiquote (UnquoteSplicing x) = do
  (t, c, x') <- infer x
  return (t, c, UnquoteSplicing x')
inferQuasiquote (Quasicons car cdr) = do
  (t1, c1, car') <- inferQuasiquote car
  (t2, c2, cdr') <- inferQuasiquote cdr
  return (TypeProduct t1 t2, c1 ++ c2, Quasicons car' cdr')

inferTop :: Environment
         -> [(Name, Expression Name)]
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
    fv (TypeProduct a b) = fv a ++ fv b
    fv (TypeSum a b) = fv a ++ fv b
    normtype (TypeArrow a b) = TypeArrow (normtype a) (normtype b)
    normtype (TypeSymbol a) = TypeSymbol a
    normtype (TypeProduct a b) = TypeProduct (normtype a) (normtype b)
    normtype (TypeSum a b) = TypeSum (normtype a) (normtype b)
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
