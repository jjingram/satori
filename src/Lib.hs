module Lib
    ( parseExpression
    , parseToplevel
    , codegen
    , emptyModule
    ) where

import Text.Parsec

import LLVM.General.Module
import LLVM.General.Context
import qualified LLVM.General.AST as AST

import Parser
import Syntax
import Codegen
import Emit

parseExpression :: String -> Either ParseError Expression
parseExpression s = parse (contents expression) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expression]
parseToplevel s = parse (contents toplevel) "<stdin>" s

emptyModule :: String -> AST.Module
emptyModule label = AST.defaultModule { AST.moduleName = label }

codegen :: AST.Module -> [Expression] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn
