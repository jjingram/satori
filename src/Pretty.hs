{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pretty
  ( ppconstraint
  , ppconstraints
  , ppenv
  , ppscheme
  , ppsubst
  , ppsignature
  , pptype
  ) where

import Environment
import Infer
import Syntax
import Type

import qualified Data.Map as Map
import Text.PrettyPrint

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Name where
  ppr _ x = text x

instance Pretty TypeVariable where
  ppr _ (TV x) = text x

instance Pretty Type where
  ppr p (TypeArrow a b) =
    (parensIf (isArrow a) (ppr p a)) <+> text "->" <+> ppr p b
    where
      isArrow TypeArrow {} = True
      isArrow _ = False
  ppr p (TypeVariable a) = ppr p a
  ppr _ (TypeSymbol a) = text a

instance Pretty Scheme where
  ppr p (Forall [] t) = ppr p t
  ppr p (Forall ts t) =
    text "forall" <+>
    hcat (punctuate space (map (ppr p) ts)) <> text "." <+> ppr p t

instance Pretty Constraint where
  ppr p (a, b) = (ppr p a) <+> text " ~ " <+> (ppr p b)

instance Pretty [Constraint] where
  ppr p cs = vcat (punctuate space (map (ppr p) cs))

instance Pretty Subst where
  ppr p (Subst s) = vcat (punctuate space (map pprSub $ Map.toList s))
    where
      pprSub (a, b) = ppr 0 a <+> text "~" <+> ppr 0 b

instance Show TypeError where
  show (UnificationFail a b) =
    concat ["Cannot unify types: \n\t", pptype a, "\nwith \n\t", pptype b]
  show (InfiniteType (TV a) b) =
    concat ["Cannot construct the infinite type: ", a, " = ", pptype b]
  show (Ambigious cs) =
    concat
      [ "Cannot not match expected type: '" ++
      pptype a ++ "' with actual type: '" ++ pptype b ++ "'\n"
      | (a, b) <- cs
      ]
  show (UnboundVariable a) = "Not in scope: " ++ a

ppscheme :: Scheme -> String
ppscheme = render . ppr 0

pptype :: Type -> String
pptype = render . ppr 0

ppsignature :: (String, Scheme) -> String
ppsignature (a, b) = a ++ " : " ++ ppscheme b

ppenv :: Environment -> [String]
ppenv (TypeEnvironment env) = map ppsignature $ Map.toList env

ppconstraint :: Constraint -> String
ppconstraint = render . ppr 0

ppconstraints :: [Constraint] -> String
ppconstraints = render . ppr 0

ppsubst :: Subst -> String
ppsubst = render . ppr 0
