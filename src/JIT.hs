module JIT where

import Foreign.Ptr (FunPtr, castFunPtr)

import Control.Monad.Except

import qualified LLVM.General.AST as AST
import LLVM.General.Context
import qualified LLVM.General.ExecutionEngine as EE
import LLVM.General.Module as Mod
import LLVM.General.PassManager

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

runJIT :: AST.Module -> IO (Either String AST.Module)
runJIT mod =
  withContext $ \context ->
    jit context $ \executionEngine ->
      runExceptT $
      withModuleFromAST context mod $ \m ->
        withPassManager passes $ \pm -> do
          _ <- runPassManager pm m
          optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          putStrLn s
          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee (AST.Name "main")
            case mainfn of
              Just fn -> do
                res <- run fn
                putStrLn $ "Evaluated to: " ++ show res
              Nothing -> return ()
          return optmod
