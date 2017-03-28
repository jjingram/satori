module Emit where

import qualified Data.Map as Map

import LLVM.General.Context
import LLVM.General.Module

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C

import Control.Monad.Except

import Codegen
import Core
import Syntax

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (integer, AST.Name x))

codegenTop :: Core.Top Typed -> LLVM ()
codegenTop (Core.Define name params body) = define integer x [] bls
  where
    (x, _) = name
    bls =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        _ <-
          forM params $ \a -> do
            var <- alloca integer
            _ <- store var (local (AST.Name a))
            assign a var
        cgen body >>= ret
codegenTop (Core.Declare name args) = declare integer name fnargs
  where
    fnargs = toSig args
codegenTop (Core.Command expr) = define integer "main" [] blks
  where
    blks =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        cgen expr >>= ret

binops :: Map.Map Op (AST.Operand -> AST.Operand -> Codegen AST.Operand)
binops =
  Map.fromList [(Add, add), (Sub, sub), (Mul, mul), (SDiv, sdiv), (ILT, srem)]

cgen :: Core.Expression Typed -> Codegen AST.Operand
cgen (Core.Quote (Atom (Integer n))) = return $ constant $ C.Int 64 n
cgen (Core.BinOp op a b) =
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    -- Should never be reached.
    Nothing -> error "No such operator"
cgen (Core.Variable x r) = do
  let (name, _) = x
  x' <- getvar name
  load x'
cgen (Core.Call fn args) = do
  cfn <- cgen fn
  let arg = head args
  carg <- cgen arg
  call cfn carg

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> Core.Program Typed -> IO AST.Module
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
