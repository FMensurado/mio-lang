module IR where

type Operator = String

type Src = String

type Result = String

data Quad =
  Exp Operator
      Src
      Src
      Result

instance Show Quad where
  show (Exp op x y dist) = op ++ ": " ++ x ++ ", " ++ y ++ ", " ++ dist
