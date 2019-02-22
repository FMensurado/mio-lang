module AST where

import Control.Monad (void)
import Data.List (intercalate)
import Text.Parsec

type Name = String

-- TODO: function type & variant types
data Typing
  = TNat
  | TFloat
  | TString
  | TUnit
  | TBool
  | TId Name
  | TFn [Typing]
        Typing
  deriving (Show)

data Operator
  = Add
  | Sub
  | Mul
  | Div

instance Show Operator where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data Lit
  = LNat Int
  | LBool Bool
  | LUnit
  | LId Name
  | LOp Operator
        Lit
        Lit
  | LApp Name
         [Lit]

instance Show Lit where
  show (LNat a) = show a
  show (LBool a) = show a
  show LUnit = "unit"
  show (LId a) = a
  show (LOp o a b) = "( " ++ show a ++ show o ++ show b ++ " )"
  show (LApp name xs) = name ++ "(" ++ intercalate ", " (map show xs) ++ ")"

type Param = (String, Typing)

data Expr
  = Let { letName :: Name
        , letType :: Typing
        , letBody :: Lit }
  | Type { typeName :: Name
         , typeBody :: Typing }
  | Proc Lit
  | Fn { fnName :: Name
       , fnParams :: [Param]
       , fnReturnType :: Typing
       , fnBody :: [Expr]
       , fnReturn :: Expr }

instance Show Expr where
  show (Let name typing body) =
    "let " ++ name ++ ":" ++ show typing ++ " = " ++ show body
  show (Proc lit) = show lit
  show (Type name body) = "type " ++ name ++ " :: " ++ show body
  show (Fn name params returnType body ret) =
    let ps =
          "(" ++
          intercalate ", " (map (\(x, t) -> x ++ ":" ++ show t) params) ++ ")"
        bo =
          " { " ++
          intercalate " | " (map show body) ++ " } => [" ++ show ret ++ "]"
     in "function " ++ name ++ ps ++ " -> " ++ show returnType ++ bo

type MioParser = Parsec String ()

sep :: MioParser ()
sep = void $ oneOf " \t"

seps :: MioParser ()
seps = skipMany sep

sep1 :: MioParser ()
sep1 = skipMany1 sep

sepN :: Int -> MioParser ()
sepN n = void $ count n sep

iden :: MioParser String
iden = (:) <$> letter <*> many alphaNum
