module Eval where

import System.IO.Unsafe

import Data.Either
import Data.List (nub)
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
import Curry
import Emit
import Environment
import Infer
import Lift
import Parser
import Pretty ()
import Substitute
import Syntax
import Type

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

codegen :: AST.Module -> Program Typed -> [Type] -> IO AST.Module
codegen m fns tys =
  withContext $ \context ->
    liftError $
    withModuleFromAST context newast $ \m' -> do
      liftError $ verify m'
      return newast
  where
    modn = do
      Codegen.declare (T.ptr T.i8) "malloc" [(T.i32, AST.Name "size")]
      mapM (codegenTop tys (Map.fromList $ definitions' fns)) fns
    newast = runLLVM m modn

eval :: String -> IO (Either String Int)
eval source =
  case parseModule "<string>" source of
    Left err -> return $ Left $ show err
    Right prog -> do
      let defs = definitions prog
      let subbed = substitute (Map.fromList defs) prog
      let curried = map curryTop subbed
      let defs' = definitions curried
      case inferTop Environment.empty defs' of
        Left err -> return $ Left $ show err
        Right env -> do
          let core = constraintsTop env curried
          let corel = lefts core
          let corer = rights core
          if not (null corel)
            then return $ Left $ concatMap show corel
            else do
              let undef = filterDefinitions corer
              let (lifted, _) = lambdaLiftProgram 0 [] undef
              mod' <-
                Eval.codegen
                  (AST.defaultModule {AST.moduleName = "test"})
                  lifted
                  typeSymbols
              return $ runJIT mod'
