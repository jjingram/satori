module Emit where

import qualified Data.Map as Map

import LLVM.General.Analysis
import LLVM.General.Context
import LLVM.General.Module

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Type as T

import Control.Monad.Except

import Codegen
import Syntax
import Type

type Defs = Map.Map Name (Top Typed)

toSig :: [Name] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (llvmType $ TypeSymbol x, AST.Name x))

codegenTop :: Defs -> Top Typed -> LLVM ()
codegenTop globals (Define (name, ty) params body) =
  define
    rty'
    name'
    [(Codegen.unit, AST.UnName 0), (paramType', AST.Name param)]
    bls
  where
    name' = read name :: Word
    (TypeArrow _ rty) = ty
    rty' = llvmType rty
    (param, paramType) = head params
    paramType' =
      case paramType of
        TypeArrow a b ->
          closure $ T.ptr $ func (llvmType b) [Codegen.unit, llvmType a]
        pty -> llvmType pty
    free = tail params
    (_, types) = unzip free
    types' = T.ptr $ struct (map llvmType types)
    bls =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        var <- alloca paramType'
        _ <- store var (local (AST.Name param) paramType')
        assign param var
        let free' = local (AST.UnName 0) Codegen.unit
        freeBitCast <- bitCast types' free'
        forM_ (zip [0 .. (length free - 1)] free) $ \(idx', (n, t)) -> do
          v <-
            gep
              (T.ptr (llvmType t))
              freeBitCast
              (indices [0, fromIntegral idx'])
          assign n v
        cgen globals body >>= ret
codegenTop _ (Declare name args) = declare T.i64 name fnargs
  where
    fnargs = toSig args
codegenTop globals (Command expr) = defineMain T.i64 blks
  where
    blks =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        cgen globals expr >>= ret

binops :: Map.Map Op (AST.Operand -> AST.Operand -> AST.Type -> Codegen AST.Operand)
binops =
  Map.fromList [(Add, add), (Sub, sub), (Mul, mul), (SDiv, sdiv), (ILT, srem)]

clambda :: Top Typed -> Codegen AST.Operand
clambda (Define (name, ty) params _) = do
  let name' = read name :: Word
  let free = tail params
  let (_, types) = unzip free
  let (_, paramType) = head params
  let paramType' =
        case paramType of
          TypeArrow a b ->
            closure $ T.ptr $ func (llvmType b) [Codegen.unit, llvmType a]
          pty -> llvmType pty
  let (TypeArrow _ rty) = ty
  let rty' = llvmType rty
  let freeType = struct (map llvmType types)
  var <- malloc freeType (fromIntegral (length types))
  forM_ (zip [0 .. (length free - 1)] free) $ \(idx', (n, fty)) -> do
    v <- getvar n
    case v of
      Nothing -> error $ "variable not in scope: " ++ name
      Just v' -> do
        v'' <- load (llvmType fty) v'
        let fty' = llvmType fty
        ptr <- gep (T.ptr fty') var (indices [0, fromIntegral idx'])
        store ptr v''
  let fnType = func rty' [Codegen.unit, paramType']
  let fnPtrType = T.ptr fnType
  let closureType = struct [fnPtrType, Codegen.unit]
  closurePtr <- malloc closureType 2
  fnPtrPtr <- gep (T.ptr fnPtrType) closurePtr (indices [0, 0])
  _ <- store fnPtrPtr (externf (AST.UnName name') fnType)
  freePtrPtr <- gep (T.ptr fnPtrType) closurePtr (indices [0, 1])
  freeBitCast <- bitCast Codegen.unit var
  _ <- store freePtrPtr freeBitCast
  return closurePtr
clambda _ = error "not a lambda definition"

cgen :: Defs -> Expression Typed -> Codegen AST.Operand
cgen _ (Quote (Atom (Integer n))) = return $ constant $ C.Int 64 n
cgen globals x@(BinOp op a b) = do
  let f = binops Map.! op
  let t = typeOf x
  ca <- cgen globals a
  cb <- cgen globals b
  f ca cb (llvmType t)
cgen globals (Variable x) = do
  let (name, t) = x
  x' <- getvar name
  case x' of
    Just x'' -> load (llvmType t) x''
    Nothing ->
      case Map.lookup name globals of
        Just def -> clambda def
        _ -> error $ "variable not in scope: " ++ name
cgen _ Lambda {} = error "lambda lifting unsuccessful"
cgen globals (Call fn args) = do
  let arg = head args
  carg <- cgen globals arg
  let t = typeOf fn
  closurePtr <- cgen globals fn
  fnPtrPtr <- gep Codegen.unit closurePtr (indices [0, 0])
  fnPtr <- load Codegen.unit fnPtrPtr
  freePtrPtr <- gep Codegen.unit closurePtr (indices [0, 1])
  freePtr <- load Codegen.unit freePtrPtr
  call fnPtr [freePtr, carg] (llvmType t)

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> Program Typed -> IO AST.Module
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
      mapM (codegenTop (Map.fromList $ definitions' fns)) fns
    newast = runLLVM m modn
