module Reader where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

import Data.Tree
import Data.Tree.Pretty
import Control.Monad.Trans

import Syntax


lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = []
    names = ["Skip", "Assign", "Write", "Read", "While", "IfS", "Multi", "Plus", "Minus", "Times", "Divide", "Mod","EQ1", "NEQ", "Less", "LessEQ", "GreaterEQ", "Greater", "AND", "OR", "IntVal", "BinOp", "Var"]
    style = emptyDef { Tok.reservedOpNames = ops , Tok.reservedNames = names }

ss :: Parser St
ss = multi <|> ifs <|> while <|> read <|> write <|> assign <|> skip <|> (Tok.parens lexer ss)
    where
        multi = do
            reserved "Multi"
            s1 <- ss
            s2 <- ss
            return $ Multi s1 s2
        ifs = do
            reserved "IfS"
            e <- expr
            s1 <- ss
            s2 <- ss
            return $ IfS e s1 s2
        while = do
            reserved "While"
            e <- expr
            s <- ss
            return $ While e s
        read = do
            reserved "Read"
            e <- expr
            return $ Read e
        write = do
            reserved "Write"
            e <- expr
            return $ Write e
        assign = do
            reserved "Assign"
            s <- identifier
            e <- expr
            return $ Assign s e
        skip = do
            reserved "Skip"
            return $ Skip
        expr = intval <|> var <|> binop <|> (Tok.parens lexer expr)
        intval = do
            reserved "IntVal"
            i <- integer
            return $ IntVal i
        var = do
            reserved "Var"
            char '"'
            v <- identifier
            char '"'
            return $ Var v
        binop = do
            reserved $ "BinOp"
            o <- op
            e1 <- expr
            e2 <- expr
            return $ BinOp o e1 e2
        op = (reserved "Plus" >> return Plus)
            <|> (reserved "Minus" >> return Minus)
            <|> (reserved "Times" >> return Times)
            <|> (reserved "Divide" >> return Divide)
            <|> (reserved "Mod" >> return Mod)
            <|> (reserved "EQ1" >> return EQ1)
            <|> (reserved "NEQ" >> return NEQ)
            <|> (reserved "Less" >> return Less)
            <|> (reserved "LessEQ" >> return LessEQ)
            <|> (reserved "GreaterEQ" >> return GreaterEQ)
            <|> (reserved "Greater" >> return Greater)
            <|> (reserved "AND" >> return AND)
            <|> (reserved "OR" >> return OR)
        integer = Tok.integer lexer
        identifier = Tok.identifier lexer
        reserved = Tok.reserved lexer

read_st :: String -> Either ParseError St
read_st s = parse (contents ss) "<stdin>" s where
    contents p = do
      Tok.whiteSpace lexer
      r <- p
      eof
      return r

print_st :: St -> String
print_st Skip = "skip"
print_st (Assign s e) = s ++ " := " ++ (print_expr e)
print_st (Write e) = "write " ++ (print_expr e) ++ ""
print_st (Read e) = "read " ++ (print_expr e) ++ ""
print_st (While e s) = "while " ++(print_expr e) ++ " do " ++ (print_st s)
print_st (IfS e st sf) = "if " ++ (print_expr e) ++ " then " ++ (print_st st) ++ " else " ++ (print_st sf)
print_st (Multi sl sr) = (print_st sl) ++ " ; " ++ (print_st sr)

print_expr :: Expr -> String
print_expr (IntVal i) = show i
print_expr (Var v) = v
print_expr (BinOp o el er) = (print_expr el) ++ " " ++ (print_op o) ++ " " ++ (print_expr er)

print_op :: Op -> String
print_op Plus = "+"
print_op Minus = "-"
print_op Times = "*"
print_op Divide = "/"
print_op Mod = "%"
print_op EQ1 = "=="
print_op NEQ = "!="
print_op Less = "<"
print_op LessEQ = "<="
print_op GreaterEQ = ">="
print_op Greater = ">"
print_op AND = "&&"
print_op OR = "||"

tree_st :: St -> Tree String
tree_st Skip = Node "skip" []
tree_st (Assign s e) = Node "assign" [Node s [], (tree_expr e)]
tree_st (Write e) = Node "write" [(tree_expr e)]
tree_st (Read e) = Node "read" [(tree_expr e)]
tree_st (While e s) = Node "while" [Node "condition" [(tree_expr e)]
                                  , Node "do" [(tree_st s)]]
tree_st (IfS e st sf) = Node "if" [Node "condition" [(tree_expr e)]
                                 , Node "then" [(tree_st st)]
                                 , Node "else" [(tree_st sf)]]
tree_st (Multi sl sr) = Node "multi" [(tree_st sl), (tree_st sr)]

tree_expr :: Expr -> Tree String
tree_expr (IntVal i) = Node (show i) []
tree_expr (Var v) = Node v []
tree_expr (BinOp o el er) = Node (print_op o) [(tree_expr el), (tree_expr er)]

pretty_st :: St -> String
pretty_st = drawVerticalTree . tree_st
