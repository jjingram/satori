module Core where

import Syntax
import Type

type Typed = (Name, Type)

type Free = [Typed]

type Ref = Int

type Program a = [Core.Top a]

data Top a
  = Define a
           [a]
           (Core.Expression a)
  | Declare Name
            [Name]
  | Command (Core.Expression a)
  deriving (Eq, Ord, Show)

data Expression a
  = Quote Sexp
  | Quasiquote (Core.Quasisexp a)
  | BinOp Op
          (Core.Expression a)
          (Core.Expression a)
  | Variable a
             (Maybe Ref)
  | Lambda Name
           [a]
           Type
           Free
           (Core.Expression a)
  | Let [(a, Core.Expression a)]
        (Core.Expression a)
  | If (Core.Expression a)
       (Core.Expression a)
       (Core.Expression a)
  | Call (Core.Expression a)
         [Core.Expression a]
  | Case (Core.Expression a)
         [((Name, Sexp), Core.Expression a)]
  | Fix (Core.Expression a)
  deriving (Eq, Ord, Show)

data Quasisexp a
  = Quasiatom Atom
  | Quasicons (Core.Quasisexp a)
              (Core.Quasisexp a)
  | Unquote (Core.Expression a)
  | UnquoteSplicing (Core.Expression a)
  deriving (Eq, Ord, Show)

typeOf :: Core.Expression Typed -> Type.Type
typeOf (Core.Quote (Atom Nil)) = unit
typeOf (Core.Quote (Atom (Integer _))) = i64
typeOf (Core.Quote (Atom (Symbol s))) = TypeSymbol s
typeOf (Core.Quote (Cons car cdr)) =
  TypeProduct (typeOf (Core.Quote car)) (typeOf (Core.Quote cdr))
typeOf (Core.Quasiquote x) = typeOfQuasiquote x
  where
    typeOfQuasiquote :: Core.Quasisexp Typed -> Type.Type
    typeOfQuasiquote (Core.Quasiatom x') = typeOf (Core.Quote (Atom x'))
    typeOfQuasiquote (Core.Quasicons car cdr) =
      TypeProduct (typeOfQuasiquote car) (typeOfQuasiquote cdr)
    typeOfQuasiquote (Core.Unquote x') = typeOf x'
    typeOfQuasiquote (Core.UnquoteSplicing x') = typeOf x'
typeOf (Core.BinOp _ a _) = typeOf a
typeOf (Core.Variable (_, t) _) = t
typeOf (Core.Lambda _ _ t _ _) = retty t
typeOf (Core.Let _ e) = typeOf e
typeOf (Core.If _ tr fl) =
  if ttr == tfl
    then ttr
    else TypeSum ttr tfl
  where
    ttr = typeOf tr
    tfl = typeOf fl
typeOf (Core.Call f _) = typeOf f
typeOf (Core.Case _ clauses') = types'
  where
    (_, bodies) = unzip clauses'
    types = map typeOf bodies
    types' =
      if and $ zipWith (==) types (tail types)
        then head types
        else foldr1 TypeSum types
typeOf (Core.Fix e) = typeOf e

filterPolymorphic :: Core.Program Typed -> Core.Program Typed
filterPolymorphic [] = []
filterPolymorphic (x@(Core.Define _ _ e):rest) =
  if isPolymorphic e
    then filterPolymorphic rest
    else x : filterPolymorphic rest
filterPolymorphic (x@Core.Declare {}:rest) = x : filterPolymorphic rest
filterPolymorphic (x@(Core.Command e):rest) =
  if isPolymorphic e
    then filterPolymorphic rest
    else x : filterPolymorphic rest

isPolymorphic :: Core.Expression Typed -> Bool
isPolymorphic x = isPolymorphicType $ typeOf x
  where
    isPolymorphicType :: Type -> Bool
    isPolymorphicType (TypeSymbol _) = False
    isPolymorphicType (TypeVariable _) = True
    isPolymorphicType (TypeArrow a b) =
      isPolymorphicType a && isPolymorphicType b
    isPolymorphicType (TypeProduct a b) =
      isPolymorphicType a && isPolymorphicType b
    isPolymorphicType (TypeSum a b) = isPolymorphicType a && isPolymorphicType b
