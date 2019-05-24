module Syntax where

type Name = String

type FnAnno = ([Typing], Typing)

type Param = (String, Typing)

type FnHead = ([Param], Typing)

type FnBody = ([Expr], Expr)

data Typing
  = TNat
  | TFloat
  | TString
  | TUnit
  | TBool
  | TId Name
  | TFn FnAnno
  deriving (Show, Eq, Ord)

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show, Eq, Ord)

data Expr
  = Nat Integer
  | Float Double
  | Boolean Bool
  | Unit
  | BinOp Op
          Expr
          Expr
  | Let Name
        Typing
        Expr
  | Type Name
         Typing
  | App Name
        [Expr]
  | Fn Name
       FnHead
       FnBody
  deriving (Show, Eq, Ord)
