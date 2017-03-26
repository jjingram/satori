{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Context
import LLVM.General.Module

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C

import Control.Monad.Except

import Codegen
import qualified Syntax as S

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (integer, AST.Name x))

codegenTop :: S.Toplevel -> LLVM ()
codegenTop (S.Define name args body) = define integer name fnargs bls
  where
    fnargs = toSig args
    bls =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        forM_ args $ \a -> do
          var <- alloca integer
          _ <- store var (local (AST.Name a))
          assign a var
        cgen body >>= ret
codegenTop (S.Declare name args) = declare integer name fnargs
  where
    fnargs = toSig args
codegenTop (S.Command expr) = define integer "main" [] blks
  where
    blks =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        cgen expr >>= ret

cgen :: S.Expression -> Codegen AST.Operand
cgen (S.Variable x) = getvar x >>= load
cgen (S.Quote (S.Atom (S.Integer n))) = return $ cons $ C.Int 64 n
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> S.Program -> IO AST.Module
codegen m fns =
  withContext $ \context ->
    liftError $
    withModuleFromAST context newast $ \m' -> do
      llstr <- moduleLLVMAssembly m'
      putStrLn llstr
      return newast
  where
    modn = mapM codegenTop fns
    newast = runLLVM m modn
