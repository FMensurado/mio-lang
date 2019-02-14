module Lib
  ( runCompiler
  ) where

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

sourceCode :: String
sourceCode = "14 + 2 - 5 + 6"

runCompiler :: IO ()
runCompiler = putStrLn sourceCode
