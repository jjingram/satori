module Emit where

import qualified Data.Map as Map

import LLVM.General.Analysis
import LLVM.General.Context
import LLVM.General.Module

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.PrettyPrint as PP

import Control.Monad.Except

import Codegen
import Core
import Syntax
import Type

toSig :: [Syntax.Name] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (llvmType $ TypeSymbol x, AST.Name x))

codegenTop :: Core.Top Typed -> LLVM ()
codegenTop (Core.Define name params body) =
  define rty x [(Codegen.unit, AST.UnName 0), (pty, AST.Name param)] bls
  where
    (x, _) = name
    rty =
      case typeOf body of
        TypeArrow a b -> T.ptr $ T.StructureType False [rty'', Codegen.unit]
          where rty'' =
                  T.ptr $
                  T.FunctionType (llvmType b) [Codegen.unit, llvmType a] False
        rty' -> llvmType rty'
    (param, paramType) = head params
    pty =
      case paramType of
        TypeArrow a b -> T.ptr $ T.StructureType False [rty'', Codegen.unit]
          where rty'' =
                  T.ptr $
                  T.FunctionType (llvmType b) [Codegen.unit, llvmType a] False
        pty' -> llvmType pty'
    free'' = tail params
    (_, types) = unzip free''
    types' = T.ptr $ T.StructureType False (map llvmType types)
    fnPtrType = T.ptr $ T.FunctionType rty [types', pty] False
    bls =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        var <- alloca pty
        _ <- store pty var (local (AST.Name param) pty)
        assign param var
        let f = tail params
        let f' = local (AST.UnName 0) Codegen.unit
        freeBitCast <- bitCast types' f'
        forM_ (zip [0 .. (length f - 1)] f) $ \(idx', (name', ty)) -> do
          v <-
            gep
              (T.ptr (llvmType ty))
              freeBitCast
              [constant $ C.Int 32 0, constant $ C.Int 32 (fromIntegral idx')]
          assign name' v
        cgen body >>= ret
codegenTop (Core.Declare name args) = declare T.i64 name fnargs
  where
    fnargs = toSig args
codegenTop (Core.Command expr) = defineMain T.i64 blks
  where
    blks =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        cgen expr >>= ret

binops :: Map.Map Core.Op (AST.Operand -> AST.Operand -> AST.Type -> Codegen AST.Operand)
binops =
  Map.fromList
    [ (Core.Add, add)
    , (Core.Sub, sub)
    , (Core.Mul, mul)
    , (Core.SDiv, sdiv)
    , (Core.ILT, srem)
    ]

cgen :: Core.Expression Typed -> Codegen AST.Operand
cgen (Core.Quote (Core.Atom (Core.Integer n))) = return $ constant $ C.Int 64 n
cgen x@(Core.BinOp op a b) = do
  let f = binops Map.! op
  let t = typeOf x
  ca <- cgen a
  cb <- cgen b
  f ca cb (llvmType t)
cgen (Core.Variable x) = do
  let (name, t) = x
  x' <- getvar name
  load (llvmType t) x'
cgen (Core.Lambda name params _ f body) = do
  let (_, types) = unzip f
  let (_, paramType) = head params
  let pty =
        case paramType of
          TypeArrow a b -> T.ptr $ T.StructureType False [rty'', Codegen.unit]
            where rty'' =
                    T.ptr $
                    T.FunctionType (llvmType b) [Codegen.unit, llvmType a] False
          pty' -> llvmType pty'
  let rty =
        case typeOf body of
          TypeArrow a b -> T.ptr $ T.StructureType False [rty'', Codegen.unit]
            where rty'' =
                    T.ptr $
                    T.FunctionType (llvmType b) [Codegen.unit, llvmType a] False
          rty' -> llvmType rty'
  let t = T.StructureType False (map llvmType types)
  var <- malloc t (fromIntegral (length types))
  forM_ (zip [0 .. (length f - 1)] f) $ \(idx', (name', fty)) -> do
    v <- getvar name'
    v' <- load (llvmType fty) v
    let fty' = llvmType fty
    ptr <-
      gep
        (T.ptr fty')
        var
        [constant $ C.Int 32 0, constant $ C.Int 32 (fromIntegral idx')]
    store fty' ptr v'
  let fnType = T.FunctionType rty [Codegen.unit, pty] False
  let fnPtrType = T.ptr fnType
  let closureType = T.StructureType False [fnPtrType, Codegen.unit]
  closurePtr <- malloc closureType 2
  fnPtrPtr <-
    gep
      (T.ptr fnPtrType)
      closurePtr
      [constant $ C.Int 32 0, constant $ C.Int 32 0]
  _ <- store fnPtrType fnPtrPtr (externf (AST.UnName name) fnType)
  freePtrPtr <-
    gep
      (T.ptr fnPtrType)
      closurePtr
      [constant $ C.Int 32 0, constant $ C.Int 32 1]
  freeBitCast <- bitCast Codegen.unit var
  _ <- store (T.ptr t) freePtrPtr freeBitCast
  return closurePtr
cgen (Core.Call fn args) = do
  let arg = head args
  carg <- cgen arg
  let t = typeOf fn
  closurePtr <- cgen fn
  fnPtrPtr <-
    gep Codegen.unit closurePtr [constant $ C.Int 32 0, constant $ C.Int 32 0]
  fnPtr <- load Codegen.unit fnPtrPtr
  freePtrPtr <-
    gep Codegen.unit closurePtr [constant $ C.Int 32 0, constant $ C.Int 32 1]
  freePtr <- load Codegen.unit freePtrPtr
  call fnPtr [freePtr, carg] (llvmType t)

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> Core.Program Typed -> IO AST.Module
codegen m fns =
  withContext $ \context ->
    liftError $
    withModuleFromAST context newast $ \m' -> do
      llstr <- moduleLLVMAssembly m'
      putStrLn llstr
      liftError $ verify m'
      return newast
  where
    modn = do
      declare (T.ptr T.i8) "malloc" [(T.i32, AST.Name "size")]
      mapM codegenTop fns
    newast = runLLVM m modn
