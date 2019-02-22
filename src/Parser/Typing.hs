module Typing where

import AST
import Text.Parsec

toBaseType :: String -> Typing
toBaseType n
  | n == "Int" = TNat
  | n == "Float" = TFloat
  | n == "String" = TString
  | n == "Bool" = TBool
  | n == "Unit" = TUnit
  | otherwise = TId n

tBase :: MioParser Typing
tBase = toBaseType <$> iden

tFn :: MioParser Typing
tFn = do
  x <- many $ try $ innerType <* sep1
  let t = (: []) <$> (innerType <* seps)
  y <- option [] t
  string "->"
  seps
  TFn (x ++ y) <$> innerType

innerType = between (char '(') (char ')') tFn <|> tBase

typing :: MioParser Typing
typing = try tFn <|> tBase
