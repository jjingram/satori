{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Environment where

import Prelude hiding (lookup)

import Syntax
import Type

import Data.Foldable hiding (toList)
import qualified Data.Map as Map
import Data.Monoid ()

newtype Environment = TypeEnvironment
  { tys :: Map.Map Name Scheme
  } deriving (Eq, Show)

empty :: Environment
empty = TypeEnvironment Map.empty

extend :: Environment -> (Name, Scheme) -> Environment
extend env (x, s) = env {tys = Map.insert x s (tys env)}

remove :: Environment -> Name -> Environment
remove (TypeEnvironment env) var = TypeEnvironment (Map.delete var env)

extends :: Environment -> [(Name, Scheme)] -> Environment
extends env xs = env {tys = Map.union (Map.fromList xs) (tys env)}

lookup :: Name -> Environment -> Maybe Scheme
lookup key (TypeEnvironment tys) = Map.lookup key tys

merge :: Environment -> Environment -> Environment
merge (TypeEnvironment a) (TypeEnvironment b) = TypeEnvironment (Map.union a b)

mergeEnvironments :: [Environment] -> Environment
mergeEnvironments = foldl' merge empty

singleton :: Name -> Scheme -> Environment
singleton x y = TypeEnvironment (Map.singleton x y)

keys :: Environment -> [Name]
keys (TypeEnvironment env) = Map.keys env

fromList :: [(Name, Scheme)] -> Environment
fromList xs = TypeEnvironment (Map.fromList xs)

toList :: Environment -> [(Name, Scheme)]
toList (TypeEnvironment env) = Map.toList env

instance Monoid Environment where
  mempty = empty
  mappend = merge
