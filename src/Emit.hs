module Emit where

import qualified Data.Map as Map

import LLVM.General.Context
import LLVM.General.Module

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Type as T

import Control.Monad.Except

import Codegen
import Core
import Syntax
import Type

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (integer, AST.Name x))

codegenTop :: Core.Top Typed -> LLVM ()
codegenTop (Core.Define name params body) =
  define (llvmType t) x [(free', "__env__"), (llvmType t', param)] bls
  where
    (x, t) = name
    bls =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        let (param, t') = head params
        let free = tail params
        let (_, types) = unzip free
        let types' = T.StructureType False (map llvmType types)
        var <- alloca t' (llvmType t')
        _ <- store t' var (local (AST.Name param) t')
        assign param var
        forM_ (tail params) $ \(name, ty) -> do
          var <- alloca ty (llvmType ty)
          store ty var (local (AST.Name name) ty)
          assign name var
        cgen body >>= ret
codegenTop (Core.Declare name args) = declare T.i64 name fnargs
  where
    fnargs = toSig args
codegenTop (Core.Command expr) = define T.i64 "main" [] blks
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
cgen (Core.BinOp op a b) = do
  let f = (Map.!) binops op
  ca <- cgen a
  cb <- cgen b
  f ca cb
cgen (Core.Variable x _) = do
  let (name, t) = x
  x' <- getvar name
  load t x'
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
