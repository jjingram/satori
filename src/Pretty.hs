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
  , ppexpr
  , pptop
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
  ppr _ = text

instance Pretty Typed where
  ppr p (name, ty) = parens $ ppr p name <+> text ":" <+> ppr p ty

instance Pretty TypeVariable where
  ppr _ (TV x) = text x

-- TODO: pretty printing for product and sum types.
isAggregate :: Type -> Bool
isAggregate TypeArrow {} = True
isAggregate TypeProduct {} = True
isAggregate TypeSum {} = True
isAggregate _ = False

instance Pretty Type where
  ppr p (TypeArrow a b) =
    parensIf (isAggregate a) (ppr p a) <+> text "->" <+> ppr p b
  ppr p (TypeProduct a b) =
    parensIf (isAggregate a) (ppr p a) <+> text "*" <+> ppr p b
  ppr p (TypeSum a b) =
    parensIf (isAggregate a) (ppr p a) <+> text "+" <+> ppr p b
  ppr p (TypeVariable a) = ppr p a
  ppr _ (TypeSymbol a) = text a

instance Pretty Scheme where
  ppr p (Forall [] t) = ppr p t
  ppr p (Forall ts t) =
    text "forall" <+>
    hcat (punctuate space (map (ppr p) ts)) <> text "." <+> ppr p t

instance Pretty Constraint where
  ppr p (a, b) = ppr p a <+> text " ~ " <+> ppr p b

instance Pretty [Constraint] where
  ppr p cs = vcat (punctuate space (map (ppr p) cs))

instance Pretty Subst where
  ppr _ (Subst s) = vcat (punctuate space (map pprSub $ Map.toList s))
    where
      pprSub (a, b) = ppr 0 a <+> text "~" <+> ppr 0 b

instance Show TypeError where
  show (UnificationFail a b) =
    concat ["cannot unify types: \n\t", pptype a, "\nwith \n\t", pptype b]
  show (InfiniteType (TV a) b) =
    concat ["cannot construct the infinite type: ", a, " = ", pptype b]
  show (Ambigious cs) =
    concat
      [ "cannot not match expected type: '" ++
      pptype a ++ "' with actual type: '" ++ pptype b ++ "'\n"
      | (a, b) <- cs
      ]
  show (UnboundVariable a) = "not in scope: " ++ a

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

instance Pretty Op where
  ppr _ Add = text "add"
  ppr _ Sub = text "sub"
  ppr _ Mul = text "mul"
  ppr _ SDiv = text "sdiv"
  ppr _ SRem = text "srem"
  ppr _ SLT = text "slt"
  ppr _ Syntax.EQ = text "eq"

toList :: Sexp -> [Sexp]
toList (Cons car (Atom Nil)) = [car]
toList sexp@(Cons _ (Atom _)) = [sexp]
toList (Cons car cdr) = car : Pretty.toList cdr
toList x = [x]

toQuasilist :: Quasisexp a -> [Quasisexp a]
toQuasilist (Quasicons car (Quasiatom Nil)) = [car]
toQuasilist sexp@(Quasicons _ (Quasiatom _)) = [sexp]
toQuasilist (Quasicons car cdr) = car : toQuasilist cdr
toQuasilist x = [x]

ppexpr
  :: Pretty a
  => Expression a -> String
ppexpr = render . ppr 0

pptop
  :: Pretty a
  => Top a -> String
pptop = render . ppr 0

binding
  :: Pretty a
  => Int -> (a, Expression a) -> Doc
binding p (name, expr) = parens $ ppr p name <+> ppr p expr

clause
  :: Pretty a
  => Int -> (Type, Expression a) -> Doc
clause p (ty, expr) = parens (ppr p ty <+> ppr p expr)

instance Pretty a =>
         Pretty (Top a) where
  ppr p (Define name params expr) =
    parens $
    text "define" <+>
    parensIf (not (null params)) (ppr p name <+> hsep (map (ppr p) params)) $$
    nest 1 (ppr p expr)
  ppr p (Declare name params) =
    parens $ text "declare" <+> ppr p name <+> hsep (map (ppr p) params)
  ppr p (Command expr) = ppr p expr

instance Pretty a =>
         Pretty (Expression a) where
  ppr _ (Quote (Atom Nil)) = text "nil"
  ppr _ (Quote (Atom (Integer n))) = text $ show n
  ppr _ (Quote (Atom (Symbol s))) = text "'" <> text s
  ppr p (Quote sexp@(Cons _ _)) =
    text "'" <> parens (hsep (map (ppr p) (Pretty.toList sexp)))
  ppr _ (Quasiquote (Quasiatom Nil)) = text "nil"
  ppr _ (Quasiquote (Quasiatom (Integer n))) = text $ show n
  ppr _ (Quasiquote (Quasiatom (Symbol s))) = text "`" <> text s
  ppr p (Quasiquote sexp@(Quasicons _ _)) =
    text "`" <> parens (hsep (map (ppr p) (toQuasilist sexp)))
  ppr p (Quasiquote (Unquote x)) = text "`," <> ppr p x
  ppr p (Quasiquote (UnquoteSplicing x)) = text "`,@" <> ppr p x
  ppr p (BinOp op a b) = parens $ ppr p op <+> ppr p a <+> ppr p b
  ppr p (Variable x) = ppr p x
  ppr p (Lambda a b) =
    parens $ text "lambda" <+> parens (hsep (map (ppr p) a)) $$ nest 1 (ppr p b)
  ppr p (Let bs e) =
    parens $
    text "let" <+>
    nest 4 (parens (vcat (map (binding p) bs))) $$ nest 1 (ppr p e)
  ppr p (If cond tr fl) =
    parens $ text "if" <+> nest 3 (vcat (map (ppr p) [cond, tr, fl]))
  ppr p (Call a b) = parens $ ppr p a <+> hsep (map (ppr p) b)
  ppr p (Case (x, e) clauses) =
    parens $
    text "case" <+>
    parens (ppr p x <+> ppr p e) $$ nest 1 (vcat (map (clause p) clauses))
  ppr p (Fix e1 e2) = parens (text "fix" <+> ppr p e1 $$ nest 4 (ppr p e2))

instance Pretty Sexp where
  ppr _ (Atom Nil) = text "nil"
  ppr _ (Atom (Integer n)) = text $ show n
  ppr _ (Atom (Symbol s)) = text s
  ppr p (Cons car cdr) = ppr p car <+> text "." <+> ppr p cdr

instance Pretty a =>
         Pretty (Quasisexp a) where
  ppr _ (Quasiatom Nil) = text "nil"
  ppr _ (Quasiatom (Integer n)) = text $ show n
  ppr _ (Quasiatom (Symbol s)) = text s
  ppr p (Quasicons car cdr) = ppr p car <+> text "." <+> ppr p cdr
  ppr p (Unquote x) = text "," <> ppr p x
  ppr p (UnquoteSplicing x) = text ",@" <> ppr p x
