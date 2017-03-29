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

toSig :: [Name] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (llvmType $ TypeSymbol x, AST.Name x))

codegenTop :: Core.Top Typed -> LLVM ()
codegenTop (Core.Define name params body) =
  define rty x [(types', AST.Name "__free__"), (pty, AST.Name param)] bls
  where
    (x, t) = name
    arrty = llvmType' t
    rty = last arrty
    pty = head arrty
    (param, t') = head params
    free = tail params
    (_, types) = unzip free
    types' = T.ptr $ T.StructureType False (map llvmType types)
    bls =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        var <- alloca pty
        _ <- store pty var (local (AST.Name param) pty)
        assign param var
        let free = tail params
        forM_ (zip [0 .. (length free - 1)] free) $ \(idx, (name', ty)) -> do
          v <-
            gep
              (T.ptr (llvmType ty))
              (local (AST.Name "__free__") types')
              [constant $ C.Int 64 0, constant $ C.Int 64 (fromIntegral idx)]
          assign name' v
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

binops :: Map.Map Op (AST.Operand -> AST.Operand -> AST.Type -> Codegen AST.Operand)
binops =
  Map.fromList [(Add, add), (Sub, sub), (Mul, mul), (SDiv, sdiv), (ILT, srem)]

cgen :: Core.Expression Typed -> Codegen AST.Operand
cgen (Core.Quote (Atom (Integer n))) = return $ constant $ C.Int 64 n
cgen x@(Core.BinOp op a b) = do
  let f = binops Map.! op
  let t = typeOf x
  ca <- cgen a
  cb <- cgen b
  f ca cb (llvmType t)
cgen (Core.Variable x _) = do
  let (name, t) = x
  x' <- getvar name
  load (llvmType t) x'
cgen (Core.Lambda name _ ty f _) = do
  let (_, types) = unzip f
  let t = T.StructureType False (map llvmType types)
  var <- alloca t
  forM_ (zip [0 .. (length f - 1)] f) $ \(idx', (name', fty)) -> do
    v <- getvar name'
    let fty' = llvmType fty
    ptr <-
      gep
        (T.ptr fty')
        var
        [constant $ C.Int 64 0, constant $ C.Int 64 (fromIntegral idx')]
    store fty' ptr v
  assign "__free__" var
  return $ externf (AST.Name name) (llvmType ty)
cgen (Core.Call fn args) = do
  cfn <- cgen fn
  let arg = head args
  carg <- cgen arg
  let t = typeOf fn
  free <- getvar "__free__"
  call cfn [free, carg] (llvmType t)

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
