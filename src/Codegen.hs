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
import LLVM.General.AST.Global as G
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.Type as T

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

define :: AST.Type -> Word -> [(AST.Type, AST.Name)] -> [BasicBlock] -> LLVM ()
define ty label argtys body =
  addDefn $
  GlobalDefinition $
  functionDefaults
  { name = UnName label
  , parameters = ([Parameter ty' nm [] | (ty', nm) <- argtys], False)
  , returnType = ty
  , basicBlocks = body
  }

defineMain :: AST.Type -> [BasicBlock] -> LLVM ()
defineMain ty body =
  addDefn $
  GlobalDefinition $
  functionDefaults
  { name = Name "main"
  , parameters = ([], False)
  , returnType = ty
  , basicBlocks = body
  }

globalVariable :: String -> AST.Type -> LLVM ()
globalVariable label ty =
  addDefn $
  GlobalDefinition $ globalVariableDefaults {name = Name label, G.type' = ty}

declare :: AST.Type -> String -> [(AST.Type, AST.Name)] -> LLVM ()
declare ty label argtys =
  addDefn $
  GlobalDefinition $
  functionDefaults
  { name = Name label
  , parameters = ([Parameter ty' nm [] | (ty', nm) <- argtys], False)
  , returnType = ty
  , basicBlocks = []
  }

llvmType :: Type.Type -> T.Type
llvmType (TypeSymbol "()") = T.StructureType False []
llvmType (TypeSymbol "unit") = T.StructureType False []
llvmType (TypeSymbol "i64") = T.i64
llvmType (TypeSymbol _) = T.ptr T.i8
llvmType (TypeVariable _) = error "type variable"
llvmType t@(TypeArrow _ _) = T.FunctionType ty tys False
  where
    types = llvmType' t
    ty = last types
    tys = init types
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

unit :: AST.Type
unit = T.ptr $ T.StructureType False []

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

instr :: AST.Type -> Instruction -> Codegen Operand
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

local :: AST.Name -> AST.Type -> Operand
local name' ty = LocalReference ty name'

global :: AST.Name -> AST.Type -> C.Constant
global name' ty = C.GlobalReference ty name'

externf :: AST.Name -> AST.Type -> Operand
externf name' ty = ConstantOperand . C.GlobalReference ty $ name'

add :: Operand -> Operand -> AST.Type -> Codegen Operand
add a b t = instr t $ AST.Add False False a b []

sub :: Operand -> Operand -> AST.Type -> Codegen Operand
sub a b t = instr t $ AST.Sub False False a b []

mul :: Operand -> Operand -> AST.Type -> Codegen Operand
mul a b t = instr t $ AST.Mul False False a b []

sdiv :: Operand -> Operand -> AST.Type -> Codegen Operand
sdiv a b t = instr t $ AST.SDiv False a b []

srem :: Operand -> Operand -> AST.Type -> Codegen Operand
srem a b t = instr t $ AST.SRem a b []

icmp :: IP.IntegerPredicate -> Operand -> Operand -> AST.Type -> Codegen Operand
icmp cond a b t = instr t $ ICmp cond a b []

ilt :: Operand -> Operand -> AST.Type -> Codegen Operand
ilt = icmp IP.ULT

constant :: C.Constant -> Operand
constant = ConstantOperand

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

call :: Operand -> [Operand] -> AST.Type -> Codegen Operand
call fn args t =
  instr t $ AST.Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: AST.Type -> Codegen Operand
alloca t = instr t $ Alloca t Nothing 0 []

store :: AST.Type -> Operand -> Operand -> Codegen Operand
store t ptr val = instr t $ Store False ptr val Nothing 0 []

load :: AST.Type -> Operand -> Codegen Operand
load t ptr = instr t $ Load False ptr Nothing 0 []

gep :: AST.Type -> Operand -> [Operand] -> Codegen Operand
gep t addr idxs = instr t $ GetElementPtr False addr idxs []

br :: AST.Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> AST.Name -> AST.Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []
