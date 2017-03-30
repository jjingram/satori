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
codegenTop (Core.Define name params body) = do
  _ <- globalVariable (show x) types'
  define rty x [(pty, AST.Name param)] bls
  where
    (x, t) = name
    arrty = llvmType' t
    rty = last arrty
    (param, paramType) = head params
    pty = llvmType paramType
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
        let f = tail params
        env <- load types' (externf (AST.Name (show x)) types')
        forM_ (zip [0 .. (length f - 1)] f) $ \(idx', (name', ty)) -> do
          v <-
            gep
              (T.ptr (llvmType ty))
              env
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
        [constant $ C.Int 32 0, constant $ C.Int 32 (fromIntegral idx')]
    store fty' ptr v
  _ <- store (T.ptr t) (externf (AST.Name $ show name) (llvmType ty)) var
  return (externf (AST.UnName name) (llvmType ty))
cgen (Core.Call fn args) = do
  let arg = head args
  carg <- cgen arg
  let t = typeOf fn
  case fn of
    Core.Variable (name, _) _ -> do
      free <- load Codegen.unit (externf (AST.Name name) Codegen.unit)
      var <- getvar name
      call var [free, carg] (llvmType t)
    Core.Lambda name _ _ _ _ -> do
      free <- load Codegen.unit (externf (AST.Name $ show name) Codegen.unit)
      call (externf (AST.UnName name) (llvmType t)) [free, carg] (llvmType t)
    call'@Core.Call {} -> cgen call'
    _ -> error $ "cannot apply " ++ show fn

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
