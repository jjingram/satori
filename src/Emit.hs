{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Context
import LLVM.General.Module

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C

import Control.Monad.Except

import Codegen
import Curry
import qualified Syntax as S

toSig :: String -> (AST.Type, AST.Name)
toSig x = (integer, AST.Name x)

codegenTop :: Top -> LLVM ()
codegenTop (Define name args body) = define integer name fnargs bls
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
codegenTop (Declare name args) = declare integer name fnargs
  where
    fnargs = toSig args
codegenTop (Command expr) = define integer "main" [] blks
  where
    blks =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        cgen expr >>= ret

cgen :: Expr -> Codegen AST.Operand
cgen (Q (S.Atom (S.Integer n))) = return $ constant $ C.Int 64 n
cgen (Lam x e) = define integer "lambda" x' bls
  where
    x' = toSig x
    bls =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        var <- alloca integer
        _ <- store var (local (AST.Name x))
        assign x var
        cgen body >>= ret
cgen (Var x) = getvar x >>= load
cgen (App fn arg) = do
  cfn <- cgen fn
  carg <- cgen arg
  call cfn carg

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> Program -> IO AST.Module
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
