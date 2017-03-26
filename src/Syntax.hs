module Syntax where

type Name = String

type Program = [Toplevel]

data Toplevel
  = Define Name
           [Name]
           Expression
  | Declare Name
            [Name]
  | Command Expression
  deriving (Eq, Ord, Show)

data Expression
  = Number Integer
  | Variable Name
  | Call Name
         [Expression]
  deriving (Eq, Ord, Show)
