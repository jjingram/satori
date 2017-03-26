module Syntax where

type Name = String

data Expression
  = Number Integer
  | Variable Name
  | Call Name
         [Expression]
  | Definition Name
               [Name]
               Expression
  | Declaration Name
                [Name]
  deriving (Eq, Ord, Show)
