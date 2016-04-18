module Main where

import Test.HUnit

import Parser (parseL)
import Opt (opt)

import Syntax

parse input = case parseL input of
    Left err -> (Write (Var "failed to parse"))
    Right ex -> ex

main = fmap (\_ -> ()) $ runTestTT $ test
    [ True ~?= True
    , opt (Write (BinOp Plus (IntVal 40) (IntVal 2))) ~?= (Write (IntVal 42))
    
    , (opt $ parse $ "while 1!=1 do write 42") ~?= Skip
    , (opt $ parse $ "while 0 do write 42") ~?= Skip
    , (opt $ parse $ "if x==42 then x:=1+1+x else x:=2+x") ~?= (parse $ "x:=2+x")
    , (opt $ parse $ "if 1==1 then write x else read x") ~?= (Write (Var "x"))
    , (opt $ parse $ "if 1!=1 then write x else read x") ~?= (Read (Var "x"))
    , (opt $ parse $ "read x; skip; write x") ~?= (parse $ "read x; write x")
    
    , (opt $ parse $ "write x+0") ~?= (Write (Var "x"))
    , (opt $ parse $ "write x-0") ~?= (Write (Var "x"))
    , (opt $ parse $ "write x*1+1") ~?= (Write (BinOp Plus (Var "x") (IntVal 1)))
    , (opt $ parse $ "write x*1+0") ~?= (Write (Var "x"))
    , (opt $ parse $ "write x*0+1") ~?= (Write (IntVal 1))
    , (opt $ parse $ "if 1 || 0 then read x else write x") ~?= (Read (Var "x"))
    , (opt $ parse $ "if 1 || x==42 then read x else write x") ~?= (Read (Var "x"))
    , (opt $ parse $ "if x==42 && 0 then read x else write x") ~?= (Write (Var "x"))
    , (opt $ parse $ "write x==42 && 0 || x==42") ~?= (parse $ "write x==42")
    
    , (opt $ parse $ "while 1< 0 do write 42") ~?= Skip
    , (opt $ parse $ "while 1<=0 do write 42") ~?= Skip
    , (opt $ parse $ "while 0>=1 do write 42") ~?= Skip
    , (opt $ parse $ "while 0> 1 do write 42") ~?= Skip
    , (opt $ parse $ "while 0==1 do write 42") ~?= Skip
    , (opt $ parse $ "while 1!=1 do write 42") ~?= Skip
    , (opt $ parse $ "write 1+1") ~?= (Write (IntVal 2))
    , (opt $ parse $ "write 42-40") ~?= (Write (IntVal 2))
    , (opt $ parse $ "write 2*2") ~?= (Write (IntVal 4))
    , (opt $ parse $ "write 2*2+1") ~?= (Write (IntVal 5))
    , (opt $ parse $ "write 42/2") ~?= (Write (IntVal 21))
    , (opt $ parse $ "write 7%5") ~?= (Write (IntVal 2))
    , (opt $ parse $ "while 2*2 != 4 do write 42") ~?= Skip
    
    , (opt $ parse $ "write x+1>12") ~?= (parse $ "write x>11")
    , (opt $ parse $ "write x-1>2") ~?= (parse $ "write x>3")
    , (opt $ parse $ "write 1+x>2") ~?= (parse $ "write x>1")
    , (opt $ parse $ "write 2-x>1") ~?= (parse $ "write 1>x")
    ]
