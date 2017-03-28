module Emit where

import qualified Data.Map as Map

import LLVM.General.Context
import LLVM.General.Module

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C

import Control.Monad.Except

import Codegen
import Core
import Environment
import Syntax

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (integer, AST.Name x))

codegenTop :: Environment -> Core.Top Typed -> LLVM ()
codegenTop env (Core.Define name params body) = define integer x [] bls
  where
    (x, _) = name
    (xs, _) = unzip params
    bls =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        _ <-
          forM xs $ \a -> do
            var <- alloca integer
            _ <- store var (local (AST.Name a))
            assign a var
        cgen env body >>= ret
codegenTop _ (Core.Declare name args) = declare integer name fnargs
  where
    fnargs = toSig args
codegenTop env (Core.Command expr) = define integer "main" [] blks
  where
    blks =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        cgen env expr >>= ret

binops :: Map.Map Op (AST.Operand -> AST.Operand -> Codegen AST.Operand)
binops =
  Map.fromList [(Add, add), (Sub, sub), (Mul, mul), (SDiv, sdiv), (ILT, srem)]

cgen :: Environment -> Core.Expression Typed -> Codegen AST.Operand
cgen _ (Core.Quote (Atom (Integer n))) = return $ constant $ C.Int 64 n
cgen env (Core.BinOp op a b) =
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen env a
      cb <- cgen env b
      f ca cb
    -- Should never be reached.
    Nothing -> error "No such operator"
cgen _ (Core.Variable x r) = do
  let (name, _) = x
  x' <- getvar name
  load x'
cgen env (Core.Call fn args) = do
  cfn <- cgen env fn
  let arg = head args
  carg <- cgen env arg
  call cfn carg

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> Core.Program Typed -> Environment -> IO AST.Module
codegen m fns env =
  withContext $ \context ->
    liftError $
    withModuleFromAST context newast $ \m' -> do
      llstr <- moduleLLVMAssembly m'
      putStrLn llstr
      return newast
  where
    modn = mapM (codegenTop env) fns
    newast = runLLVM m modn
