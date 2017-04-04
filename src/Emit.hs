module Emit where

import Data.List
import qualified Data.Map as Map
import Data.Maybe

import LLVM.General.Analysis
import LLVM.General.Context
import LLVM.General.Module

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.PrettyPrint as PP

import Control.Monad.Except

import Codegen
import Syntax
import Type

type Defs = Map.Map Name (Top Typed)

false :: AST.Operand
false = constant $ C.Int 1 0

toSig :: [Type] -> [(AST.Type, AST.Name)]
toSig = map (\(TypeSymbol x) -> (llvmType (TypeSymbol x), AST.Name x))

codegenTop :: [Type] -> Defs -> Top Typed -> LLVM ()
codegenTop tys globals (Define (name, ty) params body) =
  define
    sumType
    name'
    [(Codegen.unit, AST.UnName 0), (sumType, AST.Name param)]
    bls
  where
    name' = read name :: Word
    (param, _) = head params
    free = tail params
    freeType = T.ptr $ struct (replicate (length free) sumType)
    bls =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        assign param (local (AST.Name param) sumType)
        freeBitCast <- bitCast freeType (local (AST.UnName 0) Codegen.unit)
        forM_ (zip [0 .. (length free - 1)] free) $ \(idx', (n, _)) -> do
          v <- gep (T.ptr sumType) freeBitCast (indices [0, fromIntegral idx'])
          v' <- load sumType v
          assign n v'
        cgen tys globals body >>= ret
codegenTop _ _ (Declare name args) = declare T.i64 name fnargs
  where
    fnargs = toSig args
codegenTop tys globals (Command expr) = defineMain T.i64 blks
  where
    blks =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        body <- cgen tys globals expr
        bodyDatum <- gep (T.ptr Codegen.unit) body (indices [0, 1])
        bodyDatumBitCast <- bitCast (T.ptr $ T.ptr T.i64) bodyDatum
        resPtr <- load (T.ptr T.i64) bodyDatumBitCast
        res <- load T.i64 resPtr
        ret res

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

clambda :: [Type] -> Top Typed -> Codegen AST.Operand
clambda tys (Define (name, ty) params _) = do
  let name' = read name :: Word
  let free = tail params
  let freeType = struct (replicate (length free) sumType)
  let fnType = func sumType [Codegen.unit, sumType]
  let fnPtrType = T.ptr fnType
  let closureType = struct [fnPtrType, Codegen.unit]
  closurePtr <- malloc closureType 2
  fnPtrPtr <- gep (T.ptr fnPtrType) closurePtr (indices [0, 0])
  _ <- store fnPtrPtr (externf (AST.UnName name') fnType)
  freePtrPtr <- gep (T.ptr fnPtrType) closurePtr (indices [0, 1])
  var <- malloc freeType (fromIntegral (length free))
  freeBitCast <- bitCast Codegen.unit var
  _ <- store freePtrPtr freeBitCast
  forM_ (zip [0 .. (length free - 1)] free) $ \(idx', (n, _)) -> do
    v <- getvar n
    case v of
      Nothing -> error $ "variable not in scope: " ++ n
      Just v' -> do
        ptr <- gep (T.ptr sumType) var (indices [0, fromIntegral idx'])
        store ptr v'
  closureSum <- malloc (struct [T.i64, Codegen.unit]) 2
  closureTag <- gep (T.ptr T.i64) closureSum (indices [0, 0])
  _ <-
    store
      closureTag
      (constant $ C.Int 64 (fromIntegral . fromJust $ ty `elemIndex` tys))
  closureDatum <- gep (T.ptr Codegen.unit) closureSum (indices [0, 1])
  closurePtrBitCast <- bitCast Codegen.unit closurePtr
  _ <- store closureDatum closurePtrBitCast
  return closureSum
clambda _ _ = error "not a lambda definition"

cbinding :: [Type] -> Defs -> (Typed, Expression Typed) -> Codegen ()
cbinding tys globals ((name, _), expr) = do
  expr' <- cgen tys globals expr
  assign name expr'

cgen :: [Type] -> Defs -> Expression Typed -> Codegen AST.Operand
cgen tys _ (Quote (Atom (Integer n))) = do
  nPtr <- malloc T.i64 1
  _ <- store nPtr (constant $ C.Int 64 n)
  resPtr <- malloc (struct [T.i64, Codegen.unit]) 2
  resPtrTag <- gep (T.ptr T.i64) resPtr (indices [0, 0])
  _ <-
    store
      resPtrTag
      (constant $
       C.Int 64 (fromIntegral . fromJust $ TypeSymbol "i64" `elemIndex` tys))
  resPtrDatum <- gep (T.ptr Codegen.unit) resPtr (indices [0, 1])
  resPtrDatumBitCast <- bitCast (T.ptr $ T.ptr T.i64) resPtrDatum
  _ <- store resPtrDatumBitCast nPtr
  return resPtr
cgen tys globals (BinOp op a b) = do
  let f = binops Map.! op
  let (_, ty) = ops Map.! op
  let (TypeArrow ta (TypeArrow tb rty)) = ty
  let rty' = llvmType rty
  ca <- cgen tys globals a
  let ta' = llvmType ta
  cb <- cgen tys globals b
  let tb' = llvmType tb
  caDatum <- gep (T.ptr Codegen.unit) ca (indices [0, 1])
  caBitCast <- bitCast (T.ptr $ T.ptr ta') caDatum
  caPtr <- load (T.ptr ta') caBitCast
  lhs <- load ta' caPtr
  cbDatum <- gep (T.ptr Codegen.unit) cb (indices [0, 1])
  cbBitCast <- bitCast (T.ptr $ T.ptr tb') cbDatum
  cbPtr <- load (T.ptr tb') cbBitCast
  rhs <- load tb' cbPtr
  res <- f lhs rhs rty'
  resPtr <- malloc rty' 1
  _ <- store resPtr res
  resSumPtr <- malloc (struct [T.i64, Codegen.unit]) 2
  resPtrTag <- gep (T.ptr T.i64) resSumPtr (indices [0, 0])
  _ <-
    store
      resPtrTag
      (constant $ C.Int 64 (fromIntegral . fromJust $ rty `elemIndex` tys))
  resPtrDatum <- gep (T.ptr Codegen.unit) resSumPtr (indices [0, 1])
  resPtrDatumBitCast <- bitCast (T.ptr $ T.ptr rty') resPtrDatum
  _ <- store resPtrDatumBitCast resPtr
  return resSumPtr
cgen tys globals (Variable x) = do
  let (name, _) = x
  x' <- getvar name
  case x' of
    Just x'' -> return x''
    Nothing ->
      case Map.lookup name globals of
        Just def -> clambda tys def
        _ -> error $ "variable not in scope: " ++ name
cgen _ _ Lambda {} = error "lambda lifting unsuccessful"
cgen tys globals (Let bindings expr) = do
  _ <- mapM_ (cbinding tys globals) bindings
  cgen tys globals expr
cgen tys globals (If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  condSum <- cgen tys globals cond
  condDatumPtr <- gep (T.ptr Codegen.unit) condSum (indices [0, 1])
  condDatum <- load Codegen.unit condDatumPtr
  condDatumBitCast <- bitCast (T.ptr T.i1) condDatum
  cond' <- load T.i1 condDatumBitCast
  test <- icmp IP.NE false cond' T.i1
  _ <- cbr test ifthen ifelse
  _ <- setBlock ifthen
  trval <- cgen tys globals tr
  _ <- br ifexit
  ifthen' <- getBlock
  _ <- setBlock ifelse
  flval <- cgen tys globals fl
  _ <- br ifexit
  ifelse' <- getBlock
  _ <- setBlock ifexit
  phi sumType [(trval, ifthen'), (flval, ifelse')]
cgen tys globals (Call fn args) = do
  let arg = head args
  carg <- cgen tys globals arg
  closureSum <- cgen tys globals fn
  closureDatum <- gep (T.ptr Codegen.unit) closureSum (indices [0, 1])
  closurePtrPtr <-
    bitCast
      (T.ptr $ closure $ T.ptr $ func sumType [Codegen.unit, sumType])
      closureDatum
  closurePtr <-
    load (closure $ func sumType [Codegen.unit, sumType]) closurePtrPtr
  fnPtrPtr <- gep Codegen.unit closurePtr (indices [0, 0])
  fnPtr <- load Codegen.unit fnPtrPtr
  freePtrPtr <- gep Codegen.unit closurePtr (indices [0, 1])
  freePtr <- load Codegen.unit freePtrPtr
  call fnPtr [freePtr, carg] sumType
cgen tys globals (Case ((x, _), e) clauses) = do
  let (ts, exprs) = unzip clauses
  blks <- mapM (\(TypeSymbol s) -> addBlock ("case." ++ s)) ts
  merge <- addBlock "case.default"
  ce <- cgen tys globals e
  assign x ce
  tagPtr <- gep (T.ptr T.i64) ce (indices [0, 0])
  tag <- load T.i64 tagPtr
  tag' <- instr T.i32 $ AST.Trunc tag T.i32 []
  let idxs = map (C.Int 32 . fromIntegral . fromJust . flip elemIndex tys) ts
  let dests = zip idxs blks
  retptrptr <- malloc sumType 1
  _ <- switch tag' merge dests
  _ <-
    forM_ (zip3 ts exprs blks) $
    (\(t, expr, block) -> do
       _ <- setBlock block
       expr' <- cgen tys globals expr
       let idx = fromJust $ t `elemIndex` tys
       let idx' = constant $ C.Int 64 (fromIntegral idx)
       _ <- store retptrptr expr'
       br merge)
  _ <- setBlock merge
  load sumType retptrptr
cgen tys globals (Fix (n, _) (Variable (n', _))) = do
  let (Define (name, ty) params _) =
        fromMaybe
          (error $ "variable not in scope: " ++ name)
          (Map.lookup n' globals)
  let name' = read name :: Word
  let free = tail params
  let freeType = struct (replicate (length free) sumType)
  let fnType = func sumType [Codegen.unit, sumType]
  let fnPtrType = T.ptr fnType
  let closureType = struct [fnPtrType, Codegen.unit]
  closurePtr <- malloc closureType 2
  closureSum <- malloc (struct [T.i64, Codegen.unit]) 2
  closureTag <- gep (T.ptr T.i64) closureSum (indices [0, 0])
  _ <-
    store
      closureTag
      (constant $ C.Int 64 (fromIntegral . fromJust $ ty `elemIndex` tys))
  closureDatum <- gep (T.ptr Codegen.unit) closureSum (indices [0, 1])
  closurePtrDatum <- bitCast Codegen.unit closurePtr
  _ <- store closureDatum closurePtrDatum
  assign n closureSum
  fnPtrPtr <- gep (T.ptr fnPtrType) closurePtr (indices [0, 0])
  _ <- store fnPtrPtr (externf (AST.UnName name') fnType)
  freePtrPtr <- gep (T.ptr fnPtrType) closurePtr (indices [0, 1])
  var <- malloc freeType (fromIntegral (length free))
  freeBitCast <- bitCast Codegen.unit var
  _ <- store freePtrPtr freeBitCast
  forM_ (zip [0 .. (length free - 1)] free) $ \(idx', (n, _)) -> do
    v <- getvar n
    case v of
      Nothing -> error $ "variable not in scope: " ++ n
      Just v' -> do
        ptr <- gep (T.ptr sumType) var (indices [0, fromIntegral idx'])
        store ptr v'
  return closureSum

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> Program Typed -> [Type] -> IO AST.Module
codegen m fns tys =
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
      mapM (codegenTop tys (Map.fromList $ definitions' fns)) fns
    newast = runLLVM m modn
