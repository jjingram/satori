{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Data.Either
import Data.List (isPrefixOf, nub)
import qualified Data.Map as Map

import Control.Arrow (second)
import Control.Monad
import Control.Monad.State.Strict

import System.Console.Repline
import System.Environment
import System.Exit

import LLVM.General.AST as AST

import Codegen
import Curry
import Emit
import Environment
import Infer
import JIT
import Lift
import Parser
import Pretty
import Substitute
import Syntax
import Type

data IState = IState
  { tyctx :: Environment.Environment
  , tmctx :: [(Syntax.Name, Expression Syntax.Name)]
  , mod :: AST.Module
  , count :: Word
  }

initModule :: AST.Module
initModule = emptyModule "satori"

initState :: IState
initState = IState (Environment.empty `extends` ops') [] initModule (0 :: Word)
  where
    ops' = map (second (Forall [])) (Map.elems Syntax.binops)

type Repl a = HaskelineT (StateT IState IO) a

hoistError
  :: Show e
  => Either e a -> Repl a
hoistError (Right val) = return val
hoistError (Left err) = do
  liftIO $ print err
  abort

exec :: Bool -> String -> Repl ()
exec update source = do
  st <- get
  prog <- hoistError $ parseModule "<stdin>" source
  liftIO $ mapM_ (putStrLn . pptop) prog
  let defs = definitions prog ++ tmctx st
  let subbed = substitute (Map.fromList defs) prog
  liftIO $ mapM_ (putStrLn . pptop) subbed
  let curried = map curryTop subbed
  liftIO $ mapM_ (putStrLn . pptop) curried
  let defs' = definitions curried ++ tmctx st
  tyctx' <- hoistError $ inferTop (tyctx st) defs'
  let tyctx'' = tyctx' `mappend` tyctx st
  let core = constraintsTop tyctx'' curried
  let corel = lefts core
  let corer = rights core
  if not (null corel)
    then liftIO $ mapM_ print corel
    else do
      liftIO $ mapM_ (putStrLn . pptop) corer
      let undef = filterDefinitions corer
      let (lifted, count') = lambdaLiftProgram (Main.count st) [] undef
      liftIO $ mapM_ (putStrLn . pptop) lifted
      mod' <- liftIO $ codegen (Main.mod st) lifted typeSymbols
      res <- liftIO $ runJIT mod'
      case res of
        Left s -> do
          liftIO $ putStrLn s
          let st' =
                st
                { tyctx = tyctx''
                , Main.count = count'
                , tmctx = defs'
                , Main.mod = mod'
                }
          when update (put st')
        Right optmod -> do
          let st' =
                st
                { tyctx = tyctx''
                , Main.count = count'
                , tmctx = defs'
                , Main.mod = optmod
                }
          when update (put st')

cmd :: String -> Repl ()
cmd = exec True

browse :: [String] -> Repl ()
browse _ = do
  st <- get
  liftIO $ mapM_ putStrLn $ ppenv (tyctx st)

load :: [String] -> Repl ()
load args = do
  contents' <- liftIO $ readFile (unwords args)
  exec True contents'

typeof :: [String] -> Repl ()
typeof args = do
  st <- get
  let arg = unwords args
  case Environment.lookup arg (tyctx st) of
    Just val -> liftIO $ putStrLn $ ppsignature (arg, val)
    Nothing -> exec False arg

quit :: a -> Repl ()
quit _ = liftIO exitSuccess

defaultMatcher
  :: MonadIO m
  => [(String, CompletionFunc m)]
defaultMatcher = [(":load", fileCompleter)]

comp
  :: (Monad m, MonadState IState m)
  => WordCompleter m
comp n = do
  let cmds = [":load", ":type", ":browse", ":quit"]
  Environment.TypeEnvironment ctx <- gets tyctx
  let defs = Map.keys ctx
  return $ filter (isPrefixOf n) (cmds ++ defs)

options :: [(String, [String] -> Repl ())]
options =
  [ ("load", Main.load)
  , ("browse", browse)
  , ("quit", quit)
  , ("type", Main.typeof)
  ]

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

shell :: Repl a -> IO ()
shell pre = flip evalStateT initState $ evalRepl "? " cmd options completer pre

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> shell (return ())
    [fname] -> shell (Main.load [fname] >> browse [] >> quit ())
    ["test", fname] -> shell (Main.load [fname] >> browse [] >> quit ())
    _ -> putStrLn "invalid arguments"
