module Eval where

import System.IO.Unsafe

import Data.Either
import qualified Data.Map as Map

import Foreign.Ptr (FunPtr, castFunPtr)

import Control.Monad.Except

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Type as T
import LLVM.General.Analysis
import LLVM.General.Context
import qualified LLVM.General.ExecutionEngine as EE
import LLVM.General.Module as Mod
import LLVM.General.PassManager

import Codegen
import Core
import Curry
import Emit
import Environment
import Fix
import Infer
import Lift
import Parser
import Substitute

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int) -> IO Int

run :: FunPtr a -> IO Int
run fn = haskFun (castFunPtr fn :: FunPtr (IO Int))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 2
    model = Nothing
    ptrelim = Nothing
    fastins = Nothing

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec {optLevel = Just 3}

runJIT :: AST.Module -> Either String Int
runJIT mod' = do
  res <-
    unsafePerformIO $
    withContext $ \context ->
      jit context $ \executionEngine ->
        runExceptT $
        withModuleFromAST context mod' $ \m ->
          withPassManager passes $ \pm -> do
            _ <- runPassManager pm m
            EE.withModuleInEngine executionEngine m $ \ee -> do
              mainfn <- EE.getFunction ee (AST.Name "main")
              case mainfn of
                Just fn -> do
                  res <- run fn
                  return $ Right res
                Nothing -> return $ Left "could not find `main` function"
  case res of
    Left err -> Left err
    Right res' -> Right res'

codegen :: AST.Module -> Core.Program Typed -> IO AST.Module
codegen m fns =
  withContext $ \context ->
    liftError $
    withModuleFromAST context newast $ \m' -> do
      liftError $ verify m'
      return newast
  where
    modn = do
      Codegen.declare (T.ptr T.i8) "malloc" [(T.i32, AST.Name "size")]
      mapM codegenTop fns
    newast = runLLVM m modn

eval :: String -> IO (Either String Int)
eval source =
  case parseModule "<string>" source of
    Left err -> return $ Left $ show err
    Right prog -> do
      let prog' = map curryTop prog
      let fixed = fix' prog'
      let defs = definitions fixed
      case inferTop Environment.empty defs of
        Left err -> return $ Left $ show err
        Right env -> do
          let prog'' = substitute (Map.fromList defs) prog'
          let core = rights $ constraintsTop env prog''
          let mono = filterPolymorphic core
          let (mono', _) = lambdaLiftProgram 0 [] mono
          mod' <-
            Eval.codegen (AST.defaultModule {AST.moduleName = "test"}) mono'
          return $ runJIT mod'
