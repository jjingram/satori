{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Data.List (isPrefixOf, foldl')
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans

import System.Console.Repline
import System.Environment
import System.Exit

import LLVM.General.AST as AST

import Codegen
import Curry
import Emit
import Environment
import Infer
import Lift
import Parser
import Pretty
import Type

data IState = IState
  { tyctx :: Environment.Environment
  , tmctx :: AST.Module
  , count :: Int
  }

initModule :: AST.Module
initModule = emptyModule "satori"

initState :: IState
initState = IState (Environment.empty `extends` ops') initModule 0
  where
    ops' = map (Control.Arrow.second (Forall [])) (Map.elems ops)

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
  let prog' = map curryTop prog
  tyctx' <- hoistError $ inferTop (tyctx st) (definitions prog')
  -- 1. Replace variables with their values
  -- 2. If a definition isn't generic then convert to a Core program and
  -- compile, else go to next definition
  let (prog'', count') = lambdaLiftProgram (count st) [] prog'
  tmctx' <- liftIO $ codegen (tmctx st) prog''
  let st' = st {tyctx = tyctx' `mappend` tyctx st, count = count'}
  when update (put st')

cmd :: String -> Repl ()
cmd = exec True

browse :: [String] -> Repl ()
browse _ = do
  st <- get
  liftIO $ mapM_ putStrLn $ ppenv (tyctx st)

load :: [String] -> Repl ()
load args = do
  contents <- liftIO $ readFile (unwords args)
  exec True contents

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
