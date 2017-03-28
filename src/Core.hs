module Core where

import Syntax
import Type

type Typed = (Name, Type)

type Free = [Name]

type Ref = Int

type Program a = [Core.Top a]

data Top a
  = Define a
           [Name]
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
  | Lambda [a]
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
  deriving (Eq, Ord, Show)

data Quasisexp a
  = Quasiatom Atom
  | Quasicons (Core.Quasisexp a)
              (Core.Quasisexp a)
  | Unquote (Core.Expression a)
  | UnquoteSplicing (Core.Expression a)
  deriving (Eq, Ord, Show)

definitions' :: Core.Program Typed -> [(Name, Core.Expression Typed)]
definitions' [] = []
definitions' (Core.Define (name, _) _ expr:rest) =
  (name, expr) : definitions' rest
definitions' (_:rest) = definitions' rest
