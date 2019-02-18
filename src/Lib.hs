module Lib
  ( runCompiler
  ) where

import Control.Monad.State
import Data.List

ints = ['0' .. '9']

ops = ['+', '-']

data Exp
  = Cal String
        Exp
        Exp
  | Const Int
  | Ep
  deriving (Show)

tokenizer :: String -> [String]
tokenizer =
  foldr
    (\x (c:cs) ->
       if x `elem` ints
         then (x : c) : cs
         else if x `elem` ops
                then ["", [x], c] ++ cs
                else c : cs)
    [""]

convertBack :: [String] -> [String]
convertBack toks = l ++ r
  where
    (l, r) =
      foldl
        (\(out, cs) x ->
           if head x `elem` ops
             then case cs of
                    (y:ys) -> (out ++ [y], x : ys)
                    [] -> (out, [x])
             else (out ++ [x], cs))
        ([], [])
        toks

buildTree :: [String] -> Exp -> Exp
buildTree x Ep =
  if head (last x) `elem` ops
    then buildTree (init x) $ Cal (last x) Ep Ep
    else error "unsupported operator"
buildTree x (Cal op n Ep) =
  let y = last x
      ys = init x
   in if head y `elem` ops
        then Cal op n $ buildTree ys $ Cal y Ep Ep
        else let z = read y :: Int
              in buildTree ys $ Cal op n $ Const z
buildTree x (Cal op Ep n) =
  let y = last x
      ys = init x
   in if head y `elem` ops
        then Cal op (buildTree ys $ Cal y Ep Ep) n
        else let z = read y :: Int
              in buildTree ys $ Cal op (Const z) n
buildTree _ x = x

buildTree' :: [String] -> Exp
buildTree' = flip buildTree Ep

simpleParser :: String -> Exp
simpleParser = buildTree' . convertBack . tokenizer

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

generator :: Exp -> String
generator x =
  intercalate "\n" $
  generateMain : map ("  " ++) (generateExp' x ++ generateExit)

sourceCode :: String
sourceCode = "14 + 2 - 5"

compile :: String -> String
compile = generator . simpleParser

runCompiler :: IO ()
runCompiler = putStrLn $ compile sourceCode
