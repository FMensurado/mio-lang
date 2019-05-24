module Expr where

import Control.Monad (void)
import Text.Parsec

import AST
import Lit
import Typing

eol :: MioParser ()
eol = eof <|> void (many1 newline)

typeAlias :: MioParser Expr
typeAlias = do
  string "type" >> sep1
  name <- iden
  seps >> string "::" >> seps
  Type name <$> (typing <* seps)

assignment :: MioParser Expr
assignment = do
  string "let" >> sep1
  name <- iden
  seps >> char ':' >> seps
  t <- typing
  seps >> char '=' >> seps
  Let name t <$> (lit <* seps)

proc :: MioParser Expr
proc = Proc <$> (lit <* seps)

functionParam :: MioParser Param
functionParam = do
  name <- iden
  seps
  char ':'
  seps
  t <- innerType
  return (name, t)

functionReturn :: MioParser Expr
functionReturn = (string "=>" >> seps) *> expr

functionBody :: MioParser ([Expr], Expr)
functionBody = do
  let gap = skipMany (newline >> seps)
  gap
  body <- many $ expr <* gap
  ret <- functionReturn
  return (body, ret)

function :: MioParser Expr
function = do
  string "fn" >> sep1
  name <- option "" $ try $ iden <* sep1
  params <- many (functionParam <* many1 sep)
  string "->" >> sep1
  returnType <- innerType <* seps
  (body, ret) <- functionBody
  lookAhead eol
  return $ Fn name params returnType body ret

expr :: MioParser Expr
expr = choice [try assignment, try function, proc]

stmt :: MioParser Expr
stmt =
  choice
    [ try function
    , try (assignment <* lookAhead eol)
    , try (typeAlias <* lookAhead eol)
    , proc <* lookAhead eol
    ]

parser :: String -> Either ParseError [Expr]
parser = runParser (many $ stmt <* (newline >> spaces)) () ""
