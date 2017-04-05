module Syntax where

import qualified Data.Map as Map

import Type

type Name = String

type Typed = (Name, Type)

type Program a = [Top a]

data Top a
  = Define a
           [a]
           (Expression a)
  | Declare Name
            [Type]
  | Command (Expression a)
  deriving (Eq, Ord, Show)

data Expression a
  = Quote Sexp
  | Quasiquote (Quasisexp a)
  | BinOp BinOp
          (Expression a)
          (Expression a)
  | Variable a
  | Lambda [a]
           (Expression a)
  | Let [(a, Expression a)]
        (Expression a)
  | If (Expression a)
       (Expression a)
       (Expression a)
  | Call (Expression a)
         [Expression a]
  | Fix a
        (Expression a)
  deriving (Eq, Ord, Show)

data Quasisexp a
  = Quasiatom Atom
  | Quasicons (Quasisexp a)
              (Quasisexp a)
  | Unquote (Expression a)
  | UnquoteSplicing (Expression a)
  deriving (Eq, Ord, Show)

data Sexp
  = Atom Atom
  | Cons Sexp
         Sexp
  deriving (Eq, Ord, Show)

data Atom
  = Nil
  | Integer Integer
  | Symbol Name
  deriving (Eq, Ord, Show)

data BinOp
  = Add
  | Sub
  | Mul
  | SDiv
  | SRem
  | SLT
  | EQ
  deriving (Eq, Ord, Show)

binops :: Map.Map BinOp (Name, Type)
binops =
  Map.fromList
    [ (Add, ("add", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (Mul, ("mul", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (Sub, ("sub", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (SDiv, ("sdiv", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (SRem, ("srem", i64 `TypeArrow` (i64 `TypeArrow` i64)))
    , (SLT, ("slt", i64 `TypeArrow` (i64 `TypeArrow` i1)))
    , (Syntax.EQ, ("eq", i64 `TypeArrow` (i64 `TypeArrow` i1)))
    ]

definitions :: Program Name -> [(Name, Expression Name)]
definitions [] = []
definitions (Define name _ expr:rest) = (name, expr) : definitions rest
definitions (_:rest) = definitions rest

typeOf :: Expression Typed -> Type.Type
typeOf (Quote (Atom Nil)) = nil
typeOf (Quote (Atom (Integer _))) = i64
typeOf (Quote (Atom (Symbol s))) = TypeSymbol s
typeOf (Quote (Cons car cdr)) =
  TypeProduct (typeOf (Quote car)) (typeOf (Quote cdr))
typeOf (Quasiquote x) = typeOfQuasiquote x
typeOf (BinOp _ a _) = typeOf a
typeOf (Variable (_, t)) = t
typeOf (Lambda param e) = TypeArrow pty (typeOf e)
  where
    (_, pty) = head param
typeOf (Let _ e) = typeOf e
typeOf (If _ tr fl) =
  if ttr == tfl
    then ttr
    else TypeSum ttr tfl
  where
    ttr = typeOf tr
    tfl = typeOf fl
typeOf (Call f _) = retty $ typeOf f
typeOf (Fix (_, ty) _) = ty

typeOfQuasiquote :: Quasisexp Typed -> Type.Type
typeOfQuasiquote (Quasiatom x') = typeOf (Quote (Atom x'))
typeOfQuasiquote (Quasicons car cdr) =
  TypeProduct (typeOfQuasiquote car) (typeOfQuasiquote cdr)
typeOfQuasiquote (Unquote x') = typeOf x'
typeOfQuasiquote (UnquoteSplicing x') = typeOf x'

retty :: Type.Type -> Type.Type
retty (TypeArrow _ b) = b
retty x = x

filterPolymorphic :: Program Typed -> Program Typed
filterPolymorphic [] = []
filterPolymorphic (x@(Define _ _ e):rest) =
  if isPolymorphic e
    then filterPolymorphic rest
    else x : filterPolymorphic rest
filterPolymorphic (x@Declare {}:rest) = x : filterPolymorphic rest
filterPolymorphic (x@(Command e):rest) =
  if isPolymorphic e
    then filterPolymorphic rest
    else x : filterPolymorphic rest

isPolymorphic :: Expression Typed -> Bool
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

definitions' :: Program Typed -> [(Name, Top Typed)]
definitions' [] = []
definitions' (top@(Define (name, _) _ _):rest) = (name, top) : definitions' rest
definitions' (_:rest) = definitions' rest

types :: Program Typed -> [Type]
types [] = []
types (Define _ _ expr:rest) = types' expr ++ types rest
types (Declare _ tys:rest) = tys ++ types rest
types (Command expr:rest) = types' expr ++ types rest

types' :: Expression Typed -> [Type]
types' expr =
  case expr of
    x@Quote {} -> [typeOf x]
    (Quasiquote x) -> types'' x
      where types'' :: Quasisexp Typed -> [Type]
            types'' x'@Quasiatom {} = [typeOfQuasiquote x']
            types'' (Quasicons car cdr) = types'' car ++ types'' cdr
            types'' (Unquote x') = types' x'
            types'' (UnquoteSplicing x') = types' x'
    (BinOp _ lhs rhs) -> types' lhs ++ types' rhs
    x@Variable {} -> [typeOf x]
    (Lambda params body) -> map snd params ++ types' body
    (Let bindings body) ->
      map snd names ++ concatMap types' exprs ++ types' body
      where (names, exprs) = unzip bindings
    (If cond tr fl) -> types' cond ++ types' tr ++ types' fl
    (Call f args) -> types' f ++ concatMap types' args
    (Fix _ x) -> types' x
