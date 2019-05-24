module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

toBaseType :: String -> Typing
toBaseType n
  | n == "Int" = TNat
  | n == "Float" = TFloat
  | n == "String" = TString
  | n == "Bool" = TBool
  | n == "Unit" = TUnit
  | otherwise = TId n

baseType :: Parser Typing
baseType = toBaseType <$> identifier

fnType :: Parser Typing
fnType = do
  params <- many innerType
  reservedOp "->"
  (\n -> TFn (params, n)) <$> innerType

innerType :: Parser Typing
innerType = parens fnType <|> baseType

typing :: Parser Typing
typing = try fnType <|> baseType

binary s f = Ex.Infix (reservedOp s >> return (BinOp f))

table =
  [ [binary "*" Mul Ex.AssocLeft, binary "/" Div Ex.AssocLeft]
  , [binary "+" Add Ex.AssocLeft, binary "-" Sub Ex.AssocLeft]
  ]

int :: Parser Expr
int = Nat <$> integer

floating :: Parser Expr
floating = Float <$> float

unit :: Parser Expr
unit = Unit <$ reserved "unit"

boolean :: Parser Expr
boolean = (\x -> Boolean $ x == "true") <$> (string "true" <|> string "false")

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

typeAlias :: Parser Expr
typeAlias = do
  reserved "type"
  name <- identifier
  reservedOp "::"
  Type name <$> typing

assignment :: Parser Expr
assignment = do
  reserved "let"
  (name, t) <- functionParam
  reservedOp "="
  Let name t <$> expr

functionParam :: Parser Param
functionParam = do
  name <- identifier
  colon
  t <- innerType
  return (name, t)

functionBody :: Parser FnBody
functionBody = do
  body <- many expr
  reservedOp "=>"
  ret <- expr
  return (body, ret)

function :: Parser Expr
function = do
  reserved "fn"
  name <- identifier
  params <- many functionParam
  reservedOp "->"
  returnType <- innerType
  Fn name (params, returnType) <$> functionBody

app :: Parser Expr
app = do
  name <- identifier
  reservedOp "<-"
  body <- many expr
  return $ App name body

factor :: Parser Expr
factor =
  try floating <|> try int <|> try boolean <|> try unit <|> try assignment <|>
  try typeAlias <|>
  try function <|>
  try app <|>
  parens expr

defn :: Parser Expr
defn = try assignment <|> try typeAlias <|> try function <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel =
  many $ do
    def <- defn
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"
