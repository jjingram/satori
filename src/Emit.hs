{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Context
import LLVM.General.Module

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C

import Control.Monad.Except

import Codegen
import Lift

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (integer, AST.Name x))

codegenTop :: Lift.Top -> LLVM ()
codegenTop (Lift.Def name param body) = define integer name [] bls
  where
    bls =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        var <- alloca integer
        _ <- store var (local (AST.Name param))
        assign param var
        cgen body >>= ret
codegenTop (Lift.Decl name args) = declare integer name fnargs
  where
    fnargs = toSig args
codegenTop (Lift.Cmd expr) = define integer "main" [] blks
  where
    blks =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        cgen expr >>= ret

cgen :: Expr -> Codegen AST.Operand
cgen (Q (S.Atom (S.Integer n))) = return $ constant $ C.Int 64 n
cgen (Lam _ _) = return mzero
cgen (Var x) = getvar x >>= load
cgen (App fn arg) = do
  cfn <- cgen fn
  carg <- cgen arg
  call cfn carg

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> Lift.Program -> IO AST.Module
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
