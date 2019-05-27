{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Control.Applicative
import Control.Monad.State

import Data.ByteString.Short.Internal
import Data.Word

import qualified Data.Map as Map

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type

import qualified LLVM.AST as AST
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Linkage as L

type Names = Map.Map ShortByteString Int

type SymbolTable = [(String, Operand)]

data CodegenState = CodegenState
  { currentBlock :: Name -- Name of the active block to append to
  , blocks :: Map.Map Name BlockState -- Blocks for function
  , symtab :: SymbolTable -- Function scope symbol table
  , blockCount :: Int -- Count of basic blocks
  , count :: Word -- Count of unnamed instructions
  , names :: Names -- Name Supply
  } deriving (Show)

data BlockState = BlockState
  { idx :: Int -- Block index
  , stack :: [Named Instruction] -- Stack of instructions
  , term :: Maybe (Named Terminator) -- Block terminator
  } deriving (Show)

newtype Codegen a = Codegen
  { runCodegen :: State CodegenState a
  } deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype LLVM a =
  LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: ShortByteString -> AST.Module
emptyModule label = defaultModule {moduleName = label}

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s {moduleDefinitions = defs ++ [d]}

define :: Type -> ShortByteString -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body =
  addDefn $
  GlobalDefinition $
  functionDefaults
    { name = Name label
    , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType = retty
    , basicBlocks = body
    }

external :: Type -> ShortByteString -> [(Type, Name)] -> LLVM ()
external retty label argtys =
  addDefn $
  GlobalDefinition $
  functionDefaults
    { name = Name label
    , linkage = L.External
    , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType = retty
    , basicBlocks = []
    }

entry :: Codegen Name
entry = gets currentBlock

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

uniqueName :: ShortByteString -> Names -> (ShortByteString, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (read $ show nm ++ show ix, Map.insert nm (ix + 1) ns)

addBlock :: ShortByteString -> Codegen Name
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

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s {currentBlock = bname}
  return bname

getBlock :: Codegen Name
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
    Nothing -> error $ "No such block: " ++ show c

local :: Name -> Operand
local = LocalReference double

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference double

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s {symtab = [(var, x)] ++ lcls}

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s {count = 1 + i}
  return $ i + 1

instr :: Instruction -> Codegen Operand
instr ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk {stack = (ref := ins) : i})
  return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk {term = Just trm})
  return trm

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd noFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub noFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul noFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv noFastMathFlags a b []

br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []
