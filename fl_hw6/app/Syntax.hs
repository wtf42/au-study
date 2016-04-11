module Syntax where

type Name = String

data Expr
  = IntVal Integer
  | BinOp Op Expr Expr
  | Var String
  deriving (Eq, Ord, Show)

data St
  = Skip
  | Assign String Expr
  | Write Expr
  | Read Expr
  | While Expr St
  | IfS Expr St St
  | Multi St St
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  | Mod
  | EQ1
  | NEQ
  | Less
  | LessEQ
  | GreaterEQ
  | Greater
  | AND
  | OR
  deriving (Eq, Ord, Show)
