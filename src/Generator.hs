module Generator
  ( generator
  ) where

import Control.Monad.State
import Data.List

import Parser (Exp(..))

generateExp :: Exp -> State Int ([String], Int)
generateExp (Const num) = do
  current <- get
  let result = "li $a" ++ show (current + 1) ++ ", " ++ show num
  put (current + 1)
  return ([result], current + 1)
generateExp (Cal op l r) = do
  (ls, ln) <- generateExp l
  (rs, rn) <- generateExp r
  let cal =
        case op of
          "+" -> "add"
          "-" -> "sub"
  let res = cal ++ " $a0, " ++ "$a" ++ show ln ++ ", $a" ++ show rn
  put 0
  return (ls ++ rs ++ [res], 0)

generateMain = "main:"

generateExit = ["li $v0, 10", "syscall"]

generateExp' :: Exp -> [String]
generateExp' x = fst (evalState (generateExp x) 0)

generatePrint =
  ["li $v0, 1", "syscall", "li $v0, 11", "li $a0, '\\n'", "syscall"]

generator :: Exp -> String
generator x =
  intercalate "\n" $
  generateMain : map ("  " ++) (generateExp' x ++ generatePrint ++ generateExit)
