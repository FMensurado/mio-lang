module Generator
  ( generator
  ) where

import Control.Monad.State
import Data.List

import Parser (AST(..))

generateAST :: AST -> State Int ([String], Int)
generateAST (Const num) = do
  current <- get
  let result = "li $a" ++ show current ++ ", " ++ show num
  put $ current + 1
  return ([result], current)
generateAST (Cal op l r) = do
  (ls, ln) <- generateAST l
  (rs, rn) <- generateAST r
  let m = min ln rn
  let cal =
        case op of
          "+" -> "add"
          "-" -> "sub"
          "*" -> "mul"
  let res =
        cal ++ " $a" ++ show m ++ ", " ++ "$a" ++ show ln ++ ", $a" ++ show rn
  put $ m + 1
  return (ls ++ rs ++ [res], m)

generateMain = "main:"

generateExit = ["li $v0, 10", "syscall"]

generateAST' :: AST -> [String]
generateAST' x = fst (evalState (generateAST x) 0)

generatePrint =
  ["li $v0, 1", "syscall", "li $v0, 11", "li $a0, '\\n'", "syscall"]

generator :: AST -> String
generator x =
  intercalate "\n" $
  generateMain : map ("  " ++) (generateAST' x ++ generatePrint ++ generateExit)
