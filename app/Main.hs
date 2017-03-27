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

import Curry
import Environment
import Infer
import Parser
import Pretty

newtype IState = IState
  { tyctx :: Environment.Environment
  }

initState :: IState
initState = IState Environment.empty

type Repl a = HaskelineT (StateT IState IO) a

hoistError
  :: Show e
  => Either e a -> Repl a
hoistError (Right val) = return val
hoistError (Left err) = do
  liftIO $ print err
  abort

definitions :: Program -> [(String, Expr)]
definitions [] = []
definitions (Define name expr:rest) = (name, expr) : definitions rest
definitions (_:rest) = definitions rest

exec :: Bool -> String -> Repl ()
exec update source = do
  st <- get
  prog <- hoistError $ parseModule "<stdin>" source
  let prog' = map curryTop prog
  tyctx' <- hoistError $ inferTop (tyctx st) (definitions prog')
  let st' = st {tyctx = tyctx' `mappend` tyctx st}
  when update (put st')

cmd :: String -> Repl ()
cmd source = exec True source

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
