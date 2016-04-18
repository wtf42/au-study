module Opt where

import Data.Set

import Syntax

opt :: St -> St
opt Skip = Skip
opt (Assign s e) = Assign s (opte e)
opt (Write e) = Write $ opte e
opt (Read e) = Read $ opte e
opt s = opt_st True s

opt_st :: Bool -> St -> St
opt_st _ (While (IntVal 0) s) = Skip
opt_st True (While e s) = opt_st False (While (opte e) (opt s))
opt_st _ (IfS (IntVal c) st sf) = opt $ if c /= 0 then st else sf
opt_st _ (IfS e st sf) | st == sf = opt st
opt_st True (IfS e st sf) = opt_st False (IfS (opte e) (opt st) (opt sf))
opt_st _ (Multi Skip s2) = opt s2
opt_st _ (Multi s1 Skip) = opt s1
opt_st True (Multi s1 s2) = opt_st False (Multi (opt s1) (opt s2))
opt_st False s = s

opte :: Expr -> Expr
opte = opt_expr True

opt_expr :: Bool -> Expr -> Expr
opt_expr _ (BinOp op (IntVal i1) (IntVal i2)) = IntVal $ simplify op i1 i2
opt_expr _ (BinOp EQ1       e1 e2) | e1 == e2 = IntVal 1
opt_expr _ (BinOp NEQ       e1 e2) | e1 == e2 = IntVal 0
opt_expr _ (BinOp AND       e1 e2) | e1 == e2 = e1
opt_expr _ (BinOp OR        e1 e2) | e1 == e2 = e1
opt_expr _ (BinOp Minus     e1 e2) | e1 == e2 = IntVal 0
opt_expr _ (BinOp Mod       e1 e2) | e1 == e2 = IntVal 0
opt_expr _ (BinOp Divide    e1 e2) | e1 == e2 = IntVal 1
opt_expr o (BinOp Plus      e (IntVal 0)) = opt_expr o e
opt_expr o (BinOp Plus      (IntVal 0) e) = opt_expr o e
opt_expr o (BinOp Minus     e (IntVal 0)) = opt_expr o e
opt_expr o (BinOp Divide    e (IntVal 1)) = opt_expr o e
opt_expr _ (BinOp Times     _ (IntVal 0)) = IntVal 0
opt_expr _ (BinOp Times     (IntVal 0) _) = IntVal 0
opt_expr o (BinOp Times     e (IntVal 1)) = opt_expr o e
opt_expr o (BinOp Times     (IntVal 1) e) = opt_expr o e
opt_expr _ (BinOp AND       e (IntVal 0)) = IntVal 0
opt_expr _ (BinOp AND       (IntVal 0) e) = IntVal 0
opt_expr o (BinOp AND       e (IntVal 1)) = opt_expr o e
opt_expr o (BinOp AND       (IntVal 1) e) = opt_expr o e
opt_expr o (BinOp OR        e (IntVal 1)) = IntVal 1
opt_expr o (BinOp OR        (IntVal 1) e) = IntVal 1
opt_expr o (BinOp OR        e (IntVal 0)) = opt_expr o e
opt_expr o (BinOp OR        (IntVal 0) e) = opt_expr o e
opt_expr _ (BinOp op_c (BinOp op_r e (IntVal i1)) (IntVal i2)) | (isCmpOp op_c) && (isRevOp op_r)
         = opt_expr True $ revOp e op_r (IntVal i1) op_c (IntVal i2)
opt_expr _ (BinOp op_c (BinOp op_r (IntVal i1) e) (IntVal i2)) | (isCmpOp op_c) && (isRevOp op_r)
         = opt_expr True $ revOp (IntVal i1) op_r e op_c (IntVal i2)
opt_expr _ (BinOp op_c (IntVal i1) (BinOp op_r e (IntVal i2))) | (isCmpOp op_c) && (isRevOp op_r)
         = opt_expr True $ revOp (IntVal i1) op_c e op_r (IntVal i2)
opt_expr _ (BinOp op_c (IntVal i1) (BinOp op_r (IntVal i2) e)) | (isCmpOp op_c) && (isRevOp op_r)
         = opt_expr True $ revOp (IntVal i1) op_c (IntVal i2) op_r e
opt_expr True (BinOp o e1 e2) = opt_expr False (BinOp o (opt_expr True e1) (opt_expr True e2))
opt_expr True iv = iv
opt_expr False e = e

simplify Plus      i1 i2 = i1 + i2
simplify Minus     i1 i2 = i1 - i2
simplify Times     i1 i2 = i1 * i2
simplify Divide    i1 i2 = i1 `div` i2
simplify Mod       i1 i2 = i1 `mod` i2
simplify EQ1       i1 i2 = if i1 == i2 then 1 else 0
simplify NEQ       i1 i2 = if i1 /= i2 then 1 else 0
simplify Less      i1 i2 = if i1 <  i2 then 1 else 0
simplify LessEQ    i1 i2 = if i1 <= i2 then 1 else 0
simplify Greater   i1 i2 = if i1 >  i2 then 1 else 0
simplify GreaterEQ i1 i2 = if i1 >= i2 then 1 else 0
simplify AND       i1 i2 = if i1 /= 0 && i2 /= 0 then 1 else 0
simplify OR        i1 i2 = if i1 /= 0 || i2 /= 0 then 1 else 0

isCmpOp op = op `member` fromList [EQ1, NEQ, Less, LessEQ, Greater, GreaterEQ]
isRevOp op = op `member` fromList [Plus, Minus]
revOp a Plus  (IntVal b) op (IntVal c) = BinOp op a (IntVal $ c - b)
revOp a Minus (IntVal b) op (IntVal c) = BinOp op a (IntVal $ c + b)
revOp (IntVal a) Plus  b op (IntVal c) = BinOp op b (IntVal $ c - a)
revOp (IntVal a) Minus b op (IntVal c) = BinOp op (IntVal $ a - c) b
revOp (IntVal a) op b Plus  (IntVal c) = BinOp op (IntVal $ a - c) b
revOp (IntVal a) op b Minus (IntVal c) = BinOp op (IntVal $ a + c) b
revOp (IntVal a) op (IntVal b) Plus  c = BinOp op (IntVal $ a - b) c
revOp (IntVal a) op (IntVal b) Minus c = BinOp op c (IntVal $ b - a)
