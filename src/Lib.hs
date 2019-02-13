module Lib
  ( runCompiler
  ) where

ints = ['0' .. '9']

ops = ['+', '-']

data Exp
  = Add Exp
        Exp
  | Sub Exp
        Exp
  | Int
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
buildTree [] x = x
buildTree x Ep =
  case last x of
    "+" -> buildTree (init x) (Add Ep Ep)
    "-" -> buildTree (init x) (Sub Ep Ep)
    _ -> error "unsupported operator"

simpleParser = convertBack . tokenizer

sourceCode :: String
sourceCode = "14 + 2 - 5"

runCompiler :: IO ()
runCompiler = putStrLn sourceCode
