module Lit
  ( lit
  ) where

import Text.Parsec

import AST

op :: MioParser Operator -> MioParser (Lit -> Lit -> Lit)
op x = LOp <$> x

opAdd :: MioParser Operator
opAdd = char '+' >> return Add

opSub :: MioParser Operator
opSub = char '-' >> return Sub

opMul :: MioParser Operator
opMul = char '*' >> return Mul

opDiv :: MioParser Operator
opDiv = char '/' >> return Div

litNat :: MioParser Lit
litNat = LNat . read <$> many1 digit

litBool :: MioParser Lit
litBool = LBool . (== "true") <$> (string "true" <|> string "false")

litUnit :: MioParser Lit
litUnit = LUnit <$ string "unit"

litId :: MioParser Lit
litId = LId <$> many1 letter

litApp :: MioParser Lit
litApp = do
  name <- many1 letter
  many1 space >> string "<-"
  body <- many (many1 (oneOf " \t") *> litPa)
  return $ LApp name body

litPa :: MioParser Lit
litPa =
  try (between (char '(') (char ')') litApp) <|>
  try (between (char '(') (char ')') litExpr) <|>
  try litBool <|>
  try litUnit <|>
  try litNat <|>
  litId

litExpr :: MioParser Lit
litExpr = litTerm `chainl1` op (opAdd <|> opSub)

litTerm :: MioParser Lit
litTerm = litFactor `chainl1` op (opMul <|> opDiv)

litFactor :: MioParser Lit
litFactor =
  try (between (seps *> char '(' <* seps) (seps *> char ')' <* seps) litExpr) <|>
  (seps *> (litNat <|> litId) <* seps)

lit :: MioParser Lit
lit =
  try litBool <|> try litUnit <|> try litApp <|> try litExpr <|> try litNat <|>
  litId
