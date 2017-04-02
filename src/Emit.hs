module Emit where

import qualified Data.Map as Map

import LLVM.General.Analysis
import LLVM.General.Context
import LLVM.General.Module

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.Type as T

import Control.Monad.Except

import Codegen
import Syntax
import Type

type Defs = Map.Map Name (Top Typed)

false :: AST.Operand
false = constant $ C.Int 1 0

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
    paramType' = llvmType paramType
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
  Map.fromList
    [ (Add, add)
    , (Sub, sub)
    , (Mul, mul)
    , (SDiv, sdiv)
    , (SRem, srem)
    , (SLT, slt)
    , (Syntax.EQ, eq)
    ]

clambda :: Defs -> Top Typed -> Codegen AST.Operand
clambda globals (Define (name, ty) params _) = do
  let name' = read name :: Word
  let free = tail params
  let (_, types) = unzip free
  let (_, paramType) = head params
  let paramType' = llvmType paramType
  let (TypeArrow _ rty) = ty
  let rty' = llvmType rty
  let freeType = struct (map llvmType types)
  let fnType = func rty' [Codegen.unit, paramType']
  let fnPtrType = T.ptr fnType
  let closureType = struct [fnPtrType, Codegen.unit]
  closurePtr <- malloc closureType 2
  fnPtrPtr <- gep (T.ptr fnPtrType) closurePtr (indices [0, 0])
  _ <- store fnPtrPtr (externf (AST.UnName name') fnType)
  freePtrPtr <- gep (T.ptr fnPtrType) closurePtr (indices [0, 1])
  var <- malloc freeType (fromIntegral (length types))
  freeBitCast <- bitCast Codegen.unit var
  _ <- store freePtrPtr freeBitCast
  forM_ (zip [0 .. (length free - 1)] free) $ \(idx', (n, fty)) -> do
    v <- getvar n
    case v of
      Nothing ->
        case Map.lookup name globals of
          Just _ -> do
            ptr <-
              gep (T.ptr (llvmType fty)) var (indices [0, fromIntegral idx'])
            store ptr closurePtr
          _ -> error $ "variable not in scope: " ++ name
      Just v' -> do
        v'' <- load (llvmType fty) v'
        let fty' = llvmType fty
        ptr <- gep (T.ptr fty') var (indices [0, fromIntegral idx'])
        store ptr v''
  return closurePtr
clambda _ _ = error "not a lambda definition"

cbinding :: Defs -> (Typed, Expression Typed) -> Codegen ()
cbinding globals ((name, ty), expr) = do
  let ty' = llvmType ty
  var <- alloca ty'
  expr' <- cgen globals expr
  _ <- store var expr'
  assign name var

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
        Just def -> clambda globals def
        _ -> error $ "variable not in scope: " ++ name
cgen _ Lambda {} = error "lambda lifting unsuccessful"
cgen globals (Let bindings expr) = do
  _ <- mapM_ (cbinding globals) bindings
  cgen globals expr
cgen globals (If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  cond' <- cgen globals cond
  test <- icmp IP.NE false cond' T.i64
  _ <- cbr test ifthen ifelse
  _ <- setBlock ifthen
  trval <- cgen globals tr
  _ <- br ifexit
  ifthen' <- getBlock
  _ <- setBlock ifelse
  flval <- cgen globals fl
  _ <- br ifexit
  ifelse' <- getBlock
  _ <- setBlock ifexit
  phi T.i64 [(trval, ifthen'), (flval, ifelse')]
cgen globals (Call fn args) = do
  let arg = head args
  carg <- cgen globals arg
  let ty = typeOf fn
  closurePtr <- cgen globals fn
  fnPtrPtr <- gep Codegen.unit closurePtr (indices [0, 0])
  fnPtr <- load Codegen.unit fnPtrPtr
  freePtrPtr <- gep Codegen.unit closurePtr (indices [0, 1])
  freePtr <- load Codegen.unit freePtrPtr
  call fnPtr [freePtr, carg] (llvmType ty)
cgen globals (Fix _ e) = cgen globals e

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
