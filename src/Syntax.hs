module Syntax where

import Type

type Name = String

type Typed = (Name, Type)

type Program a = [Top a]

data Top a
  = Define a
           [a]
           (Expression a)
  | Declare Name
            [Name]
  | Command (Expression a)
  deriving (Eq, Ord, Show)

data Expression a
  = Quote Sexp
  | Quasiquote (Quasisexp a)
  | BinOp Op
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
  | Case (Expression a)
         [((a, Sexp), Expression a)]
  | Fix (Expression a)
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

data Op
  = Add
  | Sub
  | Mul
  | SDiv
  | SRem
  | ILT
  deriving (Eq, Ord, Show)

definitions :: Program Name -> [(Name, Expression Name)]
definitions [] = []
definitions (Define name [] expr:rest) = (name, expr) : definitions rest
definitions (_:rest) = definitions rest

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
free (Let binds e2) = concatMap free es ++ (free e2 `without` xs)
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

typeOf :: Expression Typed -> Type.Type
typeOf (Quote (Atom Nil)) = unit
typeOf (Quote (Atom (Integer _))) = i64
typeOf (Quote (Atom (Symbol s))) = TypeSymbol s
typeOf (Quote (Cons car cdr)) =
  TypeProduct (typeOf (Quote car)) (typeOf (Quote cdr))
typeOf (Quasiquote x) = typeOfQuasiquote x
  where
    typeOfQuasiquote :: Quasisexp Typed -> Type.Type
    typeOfQuasiquote (Quasiatom x') = typeOf (Quote (Atom x'))
    typeOfQuasiquote (Quasicons car cdr) =
      TypeProduct (typeOfQuasiquote car) (typeOfQuasiquote cdr)
    typeOfQuasiquote (Unquote x') = typeOf x'
    typeOfQuasiquote (UnquoteSplicing x') = typeOf x'
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
typeOf (Case _ clauses') = types'
  where
    (_, bodies) = unzip clauses'
    types = map typeOf bodies
    types' =
      if and $ zipWith (==) types (tail types)
        then head types
        else foldr1 TypeSum types
typeOf (Fix e) = typeOf e

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
