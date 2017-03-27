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

symbol :: String -> Type
symbol = TypeSymbol

product :: Type -> Type -> Type
product = TypeProduct

sum :: Type -> Type -> Type
sum = TypeSum
