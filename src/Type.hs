module Type where

newtype TypeVariable =
  TV String
  deriving (Eq, Ord, Show)

data Type
  = TypeVariable TypeVariable
  | TypeSymbol String
  | TypeArrow Type
              Type
  | TypeProduct Type
                Type
  | TypeSum Type
            Type
  deriving (Eq, Ord, Show)

data Scheme =
  Forall [TypeVariable]
         Type
  deriving (Eq, Ord, Show)

unit :: Type
unit = TypeSymbol "()"

i64 :: Type
i64 = TypeSymbol "i64"

i1 :: Type
i1 = TypeSymbol "i1"

nth :: Integer -> Type -> Type
nth idx t = nth' 0 t
  where
    nth' :: Integer -> Type -> Type
    nth' pos (TypeArrow a b) =
      if pos == idx
        then a
        else nth' (pos + 1) b
    nth' pos (TypeProduct a b) =
      if pos == idx
        then a
        else nth' (pos + 1) b
    nth' pos (TypeSum a b) =
      if pos == idx
        then a
        else nth' (pos + 1) b
    nth' _ _ = t
