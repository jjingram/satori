module Main where

import           Control.Monad.Trans

import           System.Console.Haskeline
import           System.Environment
import           System.IO

import qualified LLVM.General.AST         as AST

import           Codegen
import           Emit
import           Parser

initModule :: AST.Module
initModule = emptyModule "my cool jit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
    loop mod = do
      minput <- getInputLine "? "
      case minput of
        Nothing -> outputStrLn ""
        Just input -> do
          modn <- liftIO $ process mod input
          case modn of
            Just modn -> loop modn
            Nothing -> loop mod

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> processFile fname >> return ()
