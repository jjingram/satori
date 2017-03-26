module Main where

import Control.Monad
import Control.Monad.Trans

import System.Console.Haskeline
import System.Environment

import qualified LLVM.General.AST as AST

import Codegen
import Emit
import Parser

initModule :: AST.Module
initModule = emptyModule "my cool jit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseProgram source
  case res of
    Left err -> print err >> return Nothing
    Right prog -> do
      ast <- codegen modo prog
      return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
    loop m = do
      minput <- getInputLine "? "
      case minput of
        Nothing -> outputStrLn ""
        Just input -> do
          mm <- liftIO $ process m input
          case mm of
            Just m' -> loop m'
            Nothing -> loop m

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [fname] -> Control.Monad.void $ processFile fname
