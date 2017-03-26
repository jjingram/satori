{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Context
import LLVM.General.Module

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.IntegerPredicate as IP

import Control.Applicative
import Control.Monad.Except
import Data.Int
import qualified Data.Map as Map
import Data.Word

import Codegen
import qualified Syntax as S

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (integer, AST.Name x))

codegenTop :: S.Expression -> LLVM ()
codegenTop (S.Definition name args body) = do
  define integer name fnargs bls
  where
    fnargs = toSig args
    bls =
      createBlocks $
      execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        forM args $ \a -> do
          var <- alloca integer
          store var (local (AST.Name a))
          assign a var
        cgen body >>= ret
codegenTop (S.Declaration name args) = do
  declare integer name fnargs
  where
    fnargs = toSig args
codegenTop exp = do
  define integer "main" [] blks
  where
    blks =
      createBlocks $
      execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        cgen exp >>= ret

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = icmp IP.ULT a b

cgen :: S.Expression -> Codegen AST.Operand
cgen (S.Variable x) = getvar x >>= load
cgen (S.Number n) = return $ cons $ C.Int 64 n
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Expression] -> IO AST.Module
codegen mod fns =
  withContext $ \context ->
    liftError $
    withModuleFromAST context newast $ \m -> do
      llstr <- moduleLLVMAssembly m
      putStrLn llstr
      return newast
  where
    modn = mapM codegenTop fns
    newast = runLLVM mod modn
