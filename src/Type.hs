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

i1 :: Type
i1 = TypeSymbol "i1"

i64 :: Type
i64 = TypeSymbol "i64"

nth :: Type -> Int -> Type
nth t idx = nth' t 0
  where
    nth' :: Type -> Int -> Type
    nth' (TypeArrow a b) pos =
      if pos == idx
        then a
        else nth' b (pos + 1)
    nth' (TypeProduct a b) pos =
      if pos == idx
        then a
        else nth' b (pos + 1)
    nth' (TypeSum a b) pos =
      if pos == idx
        then a
        else nth' b (pos + 1)
    nth' _ _ = t
