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
codegenTop globals (Define name params body) =
  define rty x' [(Codegen.unit, AST.UnName 0), (pty, AST.Name param)] bls
  where
    (x, _) = name
    x' = read x :: Word
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
clambda (Define (name, _) params body) = do
  let name' = read name :: Word
  let f = tail params
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
  let t' = T.StructureType False (map llvmType types)
  var <- malloc t' (fromIntegral (length types))
  forM_ (zip [0 .. (length f - 1)] f) $ \(idx', (n, fty)) -> do
    v <- getvar n
    case v of
      Nothing -> error $ "variable not in scope: " ++ name
      Just v' -> do
        v'' <- load (llvmType fty) v'
        let fty' = llvmType fty
        ptr <-
          gep
            (T.ptr fty')
            var
            [constant $ C.Int 32 0, constant $ C.Int 32 (fromIntegral idx')]
        store fty' ptr v''
  let fnType = T.FunctionType rty [Codegen.unit, pty] False
  let fnPtrType = T.ptr fnType
  let closureType = T.StructureType False [fnPtrType, Codegen.unit]
  closurePtr <- malloc closureType 2
  fnPtrPtr <-
    gep
      (T.ptr fnPtrType)
      closurePtr
      [constant $ C.Int 32 0, constant $ C.Int 32 0]
  _ <- store fnPtrType fnPtrPtr (externf (AST.UnName name') fnType)
  freePtrPtr <-
    gep
      (T.ptr fnPtrType)
      closurePtr
      [constant $ C.Int 32 0, constant $ C.Int 32 1]
  freeBitCast <- bitCast Codegen.unit var
  _ <- store (T.ptr t') freePtrPtr freeBitCast
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
  fnPtrPtr <-
    gep Codegen.unit closurePtr [constant $ C.Int 32 0, constant $ C.Int 32 0]
  fnPtr <- load Codegen.unit fnPtrPtr
  freePtrPtr <-
    gep Codegen.unit closurePtr [constant $ C.Int 32 0, constant $ C.Int 32 1]
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
