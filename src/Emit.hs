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
    (_, tys') = unzip free
    tys'' = T.ptr $ struct (map llvmType tys')
    bls =
      createBlocks $
      execCodegen $ do
        entry' <- addBlock entryBlockName
        _ <- setBlock entry'
        var <- alloca paramType'
        _ <- store var (local (AST.Name param) paramType')
        assign param var
        let free' = local (AST.UnName 0) Codegen.unit
        freeBitCast <- bitCast tys'' free'
        forM_ (zip [0 .. (length free - 1)] free) $ \(idx', (n, t')) -> do
          v <-
            gep
              (T.ptr (llvmType t'))
              freeBitCast
              (indices [0, fromIntegral idx'])
          assign n v
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
        cgen tys globals expr >>= ret

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

clambda :: Top Typed -> Codegen AST.Operand
clambda (Define (name, ty) params _) = do
  let name' = read name :: Word
  let free = tail params
  let (_, tys') = unzip free
  let (_, paramType) = head params
  let paramType' = llvmType paramType
  let (TypeArrow _ rty) = ty
  let rty' = llvmType rty
  let freeType = struct (map llvmType tys')
  let fnType = func rty' [Codegen.unit, paramType']
  let fnPtrType = T.ptr fnType
  let closureType = struct [fnPtrType, Codegen.unit]
  closurePtr <- malloc closureType 2
  fnPtrPtr <- gep (T.ptr fnPtrType) closurePtr (indices [0, 0])
  _ <- store fnPtrPtr (externf (AST.UnName name') fnType)
  freePtrPtr <- gep (T.ptr fnPtrType) closurePtr (indices [0, 1])
  var <- malloc freeType (fromIntegral (length tys'))
  freeBitCast <- bitCast Codegen.unit var
  _ <- store freePtrPtr freeBitCast
  forM_ (zip [0 .. (length free - 1)] free) $ \(idx', (n, fty)) -> do
    v <- getvar n
    case v of
      Nothing -> error $ "variable not in scope: " ++ n
      Just v' -> do
        v'' <- load (llvmType fty) v'
        let fty' = llvmType fty
        ptr <- gep (T.ptr fty') var (indices [0, fromIntegral idx'])
        store ptr v''
  return closurePtr
clambda _ = error "not a lambda definition"

cbinding :: [Type] -> Defs -> (Typed, Expression Typed) -> Codegen ()
cbinding tys globals ((name, ty), expr) = do
  let ty' = llvmType ty
  var <- alloca ty'
  expr' <- cgen tys globals expr
  _ <- store var expr'
  assign name var

cgen :: [Type] -> Defs -> Expression Typed -> Codegen AST.Operand
cgen _ _ (Quote (Atom (Integer n))) = return $ constant $ C.Int 64 n
cgen tys globals x@(BinOp op a b) = do
  let f = binops Map.! op
  let t = typeOf x
  ca <- cgen tys globals a
  cb <- cgen tys globals b
  f ca cb (llvmType t)
cgen _ globals (Variable x) = do
  let (name, ty) = x
  x' <- getvar name
  case x' of
    Just x'' -> load (llvmType ty) x''
    Nothing ->
      case Map.lookup name globals of
        Just def -> clambda def
        _ -> error $ "variable not in scope: " ++ name
cgen _ _ Lambda {} = error "lambda lifting unsuccessful"
cgen tys globals (Let bindings expr) = do
  _ <- mapM_ (cbinding tys globals) bindings
  cgen tys globals expr
cgen tys globals (If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  cond' <- cgen tys globals cond
  test <- icmp IP.NE false cond' T.i64
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
  phi T.i64 [(trval, ifthen'), (flval, ifelse')]
cgen tys globals (Call fn args) = do
  let arg = head args
  carg <- cgen tys globals arg
  let ty = typeOf fn
  closurePtr <- cgen tys globals fn
  fnPtrPtr <- gep Codegen.unit closurePtr (indices [0, 0])
  fnPtr <- load Codegen.unit fnPtrPtr
  freePtrPtr <- gep Codegen.unit closurePtr (indices [0, 1])
  freePtr <- load Codegen.unit freePtrPtr
  call fnPtr [freePtr, carg] (llvmType ty)
cgen tys globals (Case x@(name, ty) clauses) = do
  let (ts, exprs) = unzip clauses
  blks <- mapM (\(TypeSymbol s) -> addBlock ("case." ++ s)) ts
  merge <- addBlock "case.default"
  cx <- cgen tys globals (Variable x)
  tag <- gep (T.ptr $ llvmType ty) cx (indices [0, 0])
  tag' <- instr T.i32 $ AST.Trunc tag T.i32 []
  datum <- gep Codegen.unit cx (indices [0, 1])
  let idxs = map (C.Int 32 . fromIntegral . fromJust . flip elemIndex tys) ts
  let dests = zip idxs blks
  retptr <- malloc (struct [T.i64, Codegen.unit]) 2
  _ <- switch tag' merge dests
  _ <-
    forM_ (zip3 ts exprs blks) $
    (\(t, expr, block) -> do
       _ <- setBlock block
       datumBitCast <- bitCast (T.ptr $ llvmType t) datum
       assign name datumBitCast
       expr' <- cgen tys globals expr
       exprBitCast <- bitCast Codegen.unit expr'
       retptrTag <- gep Codegen.unit retptr (indices [0, 0])
       retptrDatum <- gep Codegen.unit retptr (indices [0, 1])
       let idx = fromJust $ t `elemIndex` tys
       let idx' = constant $ C.Int 64 (fromIntegral idx)
       _ <- store retptrTag idx'
       _ <- store retptrDatum exprBitCast
       br merge)
  _ <- setBlock merge
  return retptr
cgen _ globals (Fix (n, _) (Variable (n', _))) = do
  let (Define (name, ty) params _) =
        fromMaybe
          (error $ "variable not in scope: " ++ name)
          (Map.lookup n' globals)
  let name' = read name :: Word
  let free = tail params
  let (_, tys) = unzip free
  let (_, paramType) = head params
  let paramType' = llvmType paramType
  let (TypeArrow _ rty) = ty
  let rty' = llvmType rty
  let freeType = struct (map llvmType tys)
  let fnType = func rty' [Codegen.unit, paramType']
  let fnPtrType = T.ptr fnType
  let closureType = struct [fnPtrType, Codegen.unit]
  closurePtr <- malloc closureType 2
  closurePtrPtr <- malloc (T.ptr closureType) 1
  _ <- store closurePtrPtr closurePtr
  -- Tie the recursive knot.
  assign n closurePtrPtr
  fnPtrPtr <- gep (T.ptr fnPtrType) closurePtr (indices [0, 0])
  _ <- store fnPtrPtr (externf (AST.UnName name') fnType)
  freePtrPtr <- gep (T.ptr fnPtrType) closurePtr (indices [0, 1])
  var <- malloc freeType (fromIntegral (length tys))
  freeBitCast <- bitCast Codegen.unit var
  _ <- store freePtrPtr freeBitCast
  forM_ (zip [0 .. (length free - 1)] free) $ \(idx', (n'', fty)) -> do
    v <- getvar n''
    case v of
      Nothing -> error $ "variable not in scope: " ++ n
      Just v' -> do
        v'' <- load (llvmType fty) v'
        let fty' = llvmType fty
        ptr <- gep (T.ptr fty') var (indices [0, fromIntegral idx'])
        store ptr v''
  return closurePtr

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
