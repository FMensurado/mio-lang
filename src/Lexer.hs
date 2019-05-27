module Lexer where

import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+", "*", "-", "/", ";", "=>", "->", "<-", "=", "::"]
    names = ["fn", "type", "unit", "let", "type", "true", "false"]
    style =
      emptyDef
        { Tok.commentLine = "//"
        , Tok.reservedOpNames = ops
        , Tok.reservedNames = names
        }

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

colon :: Parser String
colon = Tok.colon lexer

--commaSep :: Parser a -> Parser [a]
--commaSep = Tok.commaSep lexer
semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
