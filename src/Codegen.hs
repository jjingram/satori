{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import Data.Function
import Data.List
import qualified Data.Map as Map

import Control.Monad.State

import LLVM.General.AST
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant as C
import LLVM.General.AST.Global
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.Type as T

import Core
import Syntax
import Type

newtype LLVM a = LLVM
  { unLLVM :: State AST.Module a
  } deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule {moduleName = label}

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s {moduleDefinitions = defs ++ [d]}

define :: AST.Type
       -> String
       -> [(AST.Type, AST.Name)]
       -> [BasicBlock]
       -> LLVM ()
define retty label argtys body =
  addDefn $
  GlobalDefinition $
  functionDefaults
  { name = Name label
  , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType = retty
  , basicBlocks = body
  }

declare :: AST.Type -> String -> [(AST.Type, AST.Name)] -> LLVM ()
declare retty label argtys =
  addDefn $
  GlobalDefinition $
  functionDefaults
  { name = Name label
  , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType = retty
  , basicBlocks = []
  }

llvmType :: Type.Type -> T.Type
llvmType (TypeSymbol "()") = T.StructureType False []
llvmType (TypeSymbol "unit") = T.StructureType False []
llvmType (TypeSymbol "i64") = T.i64
llvmType (TypeSymbol _) = T.ptr T.i8
llvmType (TypeVariable _) = error "type variable"
llvmType t@(TypeArrow _ _) = T.FunctionType retty argtys False
  where
    types = llvmType' t
    retty = last types
    argtys = init types
llvmType t@(TypeProduct _ _) = T.StructureType False (llvmType' t)
llvmType (TypeSum _ _) = T.StructureType False [tag, datum]
  where
    tag = T.i64
    datum = T.ptr $ T.StructureType False []

llvmType' :: Type.Type -> [T.Type]
llvmType' s@(TypeSymbol _) = [llvmType s]
llvmType' (TypeVariable _) = error "type variable"
llvmType' (TypeArrow a b) = llvmType a : llvmType' b
llvmType' (TypeProduct a b) = llvmType a : llvmType' b
llvmType' t@(TypeSum _ _) = [llvmType t]

typeOf :: Core.Expression Typed -> Type.Type
typeOf (Core.Quote (Atom Nil)) = unit
typeOf (Core.Quote (Atom (Integer _))) = i64
typeOf (Core.Quote (Atom (Symbol s))) = TypeSymbol s
typeOf (Core.Quasiquote x) = typeOfQuasiquote x
  where
    typeOfQuasiquote :: Core.Quasisexp Typed -> Core.Quasisexp Typed
    typeOfQuasiquote (Core.Quasiatom x) = typeOf x
    typeOfQuasiquote (Core.Quasicons car cdr) =
      TypeProduct (typeOf' car) (typeOf' cdr)
    typeOfQuasiquote (Core.Unquote x) = typeOf x
    typeOfQuasiquote (Core.UnquoteSplicing x) = typeOf x
typeOf (Core.BinOp _ a _) = retty $ typeOf a
typeOf (Core.Variable (_, t) _) = retty t
typeOf (Core.Lambda _ t _ _) = retty t
typeOf (Core.Let _ e) = retty $ typeOf e
typeOf (Core.If _ tr fl) =
  if ttr == tfl
    then ttr
    else TypeSum (retty $ typeOf tr) (retty $ typeOf fl)
typeOf (Core.Call f _) = retty $ typeOf f
typeOf (Core.Case _ clauses) = types'
  where
    (_, bodies) = unzip clauses
    types = (map (retty . typeOf) bodies)
    types' =
      if all (==) types
        then head types
        else foldr1 TypeSum types
typeOf (Core.Fix e) = retty $ typeOf e

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix + 1) ns)

type SymbolTable = [(String, Operand)]

data CodegenState = CodegenState
  { currentBlock :: AST.Name
  , blocks :: Map.Map AST.Name BlockState
  , symtab :: SymbolTable
  , blockCount :: Int
  , count :: Word
  , names :: Names
  } deriving (Show)

data BlockState = BlockState
  { idx :: Int
  , stack :: [Named Instruction]
  , term :: Maybe (Named Terminator)
  } deriving (Show)

newtype Codegen a = Codegen
  { runCodegen :: State CodegenState a
  } deriving (Functor, Applicative, Monad, MonadState CodegenState)

sortBlocks :: [(AST.Name, BlockState)] -> [(AST.Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (AST.Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s {count = 1 + i}
  return $ i + 1

instr :: Type.Type -> Instruction -> Codegen Operand
instr ty ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk {stack = i ++ [ref := ins]})
  return $ local ref ty

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk {term = Just trm})
  return trm

entry :: Codegen AST.Name
entry = gets currentBlock

addBlock :: String -> Codegen AST.Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s ->
    s
    { blocks = Map.insert (Name qname) new bls
    , blockCount = ix + 1
    , names = supply
    }
  return (Name qname)

setBlock :: AST.Name -> Codegen AST.Name
setBlock bname = do
  modify $ \s -> s {currentBlock = bname}
  return bname

getBlock :: Codegen AST.Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s {blocks = Map.insert active new (blocks s)}

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "no such block: " ++ show c

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s {symtab = (var, x) : lcls}

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x -> return x
    Nothing -> error $ "local variable not in scope: " ++ show var

local :: AST.Name -> Type.Type -> Operand
local name ty = LocalReference (llvmType ty) name

global :: AST.Name -> Type.Type -> C.Constant
global name ty = C.GlobalReference (llvmType ty) name

externf :: AST.Name -> Type.Type -> Operand
externf name ty = ConstantOperand . C.GlobalReference (llvmType ty) $ name

add :: Operand -> Operand -> Type.Type -> Codegen Operand
add a b t = instr t $ Add False False a b []

sub :: Operand -> Operand -> Type.Type -> Codegen Operand
sub a b t = instr t $ Sub False False a b []

mul :: Operand -> Operand -> Type.Type -> Codegen Operand
mul a b t = instr t $ Mul False False a b []

sdiv :: Operand -> Operand -> Type.Type -> Codegen Operand
sdiv a b t = instr t $ SDiv False a b []

srem :: Operand -> Operand -> Type.Type -> Codegen Operand
srem a b t = instr t $ SRem a b []

icmp :: IP.IntegerPredicate
     -> Operand
     -> Operand
     -> Type.Type
     -> Codegen Operand
icmp cond a b t = instr t $ ICmp cond a b []

ilt :: Operand -> Operand -> Type.Type -> Codegen Operand
ilt = icmp IP.ULT

constant :: C.Constant -> Operand
constant = ConstantOperand

call :: Type.Type -> Operand -> Operand -> Codegen Operand
call t fn arg = instr t $ Call Nothing CC.C [] (Right fn) [(arg, [])] [] []

alloca :: Type.Type -> AST.Type -> Codegen Operand
alloca t ty = instr t $ Alloca ty Nothing 0 []

store :: Type.Type -> Operand -> Operand -> Codegen Operand
store t ptr val = instr t $ Store False ptr val Nothing 0 []

load :: Type.Type -> Operand -> Codegen Operand
load t ptr = instr t $ Load False ptr Nothing 0 []

br :: AST.Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> AST.Name -> AST.Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []
