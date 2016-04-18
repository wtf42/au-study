module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

table = [[binary "*"  Times  Ex.AssocLeft,
          binary "/"  Divide Ex.AssocLeft,
          binary "%"  Mod    Ex.AssocLeft]
        ,[binary "+"  Plus   Ex.AssocLeft,
          binary "-"  Minus  Ex.AssocLeft]
        ,[binary "==" EQ1    Ex.AssocLeft,
          binary "!=" NEQ    Ex.AssocLeft,
          binary "<"  Less   Ex.AssocLeft,
          binary "<=" LessEQ Ex.AssocLeft,
          binary ">=" GreaterEQ Ex.AssocLeft,
          binary ">"  Greater Ex.AssocLeft]
        ,[binary "&&" AND    Ex.AssocLeft,
          binary "||" OR     Ex.AssocLeft]]

int :: Parser Expr
int = do
  n <- integer
  return $ IntVal n

variable :: Parser Expr
variable = do
  var <- identifier
  return $ Var var

factor :: Parser Expr
factor = int <|> variable

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

skip :: Parser St
skip = do
  reserved "skip"
  return $ Skip

read1 :: Parser St
read1 = do
  reserved "read"
  e <- expr
  return $ Read e

write :: Parser St
write = do
  reserved "write"
  e <- expr
  return $ Write e

assign :: Parser St
assign = do
  n <- identifier
  reservedOp ":="
  e <- expr
  return $ Assign n e

while :: Parser St
while = do
  reserved "while"
  e <- expr
  reserved "do"
  s <- st
  return $ While e s

ifs :: Parser St
ifs = do
  reserved "if"
  c <- expr
  reserved "then"
  t <- st
  reserved "else"
  f <- st
  return $ IfS c t f

st :: Parser St
st = skip
  <|> read1
  <|> write
  <|> assign
  <|> while
  <|> ifs

ss :: Parser St
ss = do { l <- st; t l }
  where t l = (do
                reservedOp ";"
                r <- ss
                return $ Multi l r)
              <|> return l

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseL :: String -> Either ParseError St
parseL s = parse (contents ss) "<stdin>" s

