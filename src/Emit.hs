module Emit where

import Control.Monad.Except
import Data.ByteString.Short.Internal

import qualified Data.Map as Map

import LLVM.AST.Type
import LLVM.Module

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import Codegen
import qualified Syntax as S

toSig :: [ShortByteString] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Fn name head body) = do
  define double name fnargs bls
  where
    fnargs = toSig args
    bls =
      createBlocks $
      execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        forM args $ \a -> do
          var <- alloca double
          store var (local (AST.Name a))
          assign a var
        cgen body >>= ret
codegenTop exp = do
  define double "main" [] blks
  where
    blks =
      createBlocks $
      execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        cgen exp >>= ret

binops = Map.fromList [("+", fadd), ("-", fsub), ("*", fmul), ("/", fdiv)]

cgen :: S.Expr -> Codegen AST.Operand
--cgen (S.BinOp "=" (S.Var var) val) = do
--  a <- getvar var
--  cval <- cgen val
--  store a cval
--  return cval
cgen (S.BinOp op a b) = do
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.App fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns =
  withContext $ \context ->
    liftError $
    withModuleFromAST context newast $ \m -> do
      llstr <- moduleLLVMAssembly m
      putStrLn llstr
      return newast
  where
    modn = mapM codegenTop fns
    newast = runLLVM mod modn
