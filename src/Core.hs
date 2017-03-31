module Core where

import Syntax
import Type

type Typed = (Name, Type)

type Free = [Typed]

type Ref = Int

type Program a = [Core.Top a]

data Top a
  = Define (Word, Type)
           [a]
           (Core.Expression a)
  | Declare Name
            [Name]
  | Command (Core.Expression a)
  deriving (Eq, Ord, Show)

data Expression a
  = Quote Core.Sexp
  | Quasiquote (Core.Quasisexp a)
  | BinOp Core.Op
          (Core.Expression a)
          (Core.Expression a)
  | Variable a
  | Lambda Word
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
         [((Name, Core.Sexp), Core.Expression a)]
  | Fix (Core.Expression a)
  deriving (Eq, Ord, Show)

data Quasisexp a
  = Quasiatom Core.Atom
  | Quasicons (Core.Quasisexp a)
              (Core.Quasisexp a)
  | Unquote (Core.Expression a)
  | UnquoteSplicing (Core.Expression a)
  deriving (Eq, Ord, Show)

data Sexp
  = Atom Core.Atom
  | Cons Core.Sexp
         Core.Sexp
  deriving (Eq, Ord, Show)

data Atom
  = Nil
  | Integer Integer
  | Symbol Name
  deriving (Eq, Ord, Show)

data Op
  = Add
  | Sub
  | Mul
  | SDiv
  | SRem
  | ILT
  deriving (Eq, Ord, Show)

coreSexp :: Syntax.Sexp -> Core.Sexp
coreSexp (Syntax.Atom Syntax.Nil) = Core.Atom Core.Nil
coreSexp (Syntax.Atom (Syntax.Integer n)) = Core.Atom (Core.Integer n)
coreSexp (Syntax.Atom (Syntax.Symbol s)) = Core.Atom (Core.Symbol s)
coreSexp (Syntax.Cons car cdr) = Core.Cons (coreSexp car) (coreSexp cdr)

coreOp :: Syntax.Op -> Core.Op
coreOp Syntax.Add = Core.Add
coreOp Syntax.Mul = Core.Mul
coreOp Syntax.Sub = Core.Sub
coreOp Syntax.SDiv = Core.SDiv
coreOp Syntax.SRem = Core.SRem
coreOp Syntax.ILT = Core.ILT

typeOf :: Core.Expression Typed -> Type.Type
typeOf (Core.Quote (Core.Atom Core.Nil)) = unit
typeOf (Core.Quote (Core.Atom (Core.Integer _))) = i64
typeOf (Core.Quote (Core.Atom (Core.Symbol s))) = TypeSymbol s
typeOf (Core.Quote (Core.Cons car cdr)) =
  TypeProduct (typeOf (Core.Quote car)) (typeOf (Core.Quote cdr))
typeOf (Core.Quasiquote x) = typeOfQuasiquote x
  where
    typeOfQuasiquote :: Core.Quasisexp Typed -> Type.Type
    typeOfQuasiquote (Core.Quasiatom x') = typeOf (Core.Quote (Core.Atom x'))
    typeOfQuasiquote (Core.Quasicons car cdr) =
      TypeProduct (typeOfQuasiquote car) (typeOfQuasiquote cdr)
    typeOfQuasiquote (Core.Unquote x') = typeOf x'
    typeOfQuasiquote (Core.UnquoteSplicing x') = typeOf x'
typeOf (Core.BinOp _ a _) = typeOf a
typeOf (Core.Variable (_, t)) = t
typeOf (Core.Lambda _ param t _ _) = TypeArrow pty t
  where
    (_, pty) = head param
typeOf (Core.Let _ e) = typeOf e
typeOf (Core.If _ tr fl) =
  if ttr == tfl
    then ttr
    else TypeSum ttr tfl
  where
    ttr = typeOf tr
    tfl = typeOf fl
typeOf (Core.Call f _) = retty $ typeOf f
typeOf (Core.Case _ clauses') = types'
  where
    (_, bodies) = unzip clauses'
    types = map typeOf bodies
    types' =
      if and $ zipWith (==) types (tail types)
        then head types
        else foldr1 TypeSum types
typeOf (Core.Fix e) = typeOf e

retty :: Type.Type -> Type.Type
retty (TypeArrow _ b) = b
retty x = x

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
