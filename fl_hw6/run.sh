#!/bin/bash

cabal run hw_tests

cabal run hw_parse < ex/ex1.txt

cabal run hw_print < ex/ex_out.txt

cabal run hw_opt < ex/ex1.txt
cabal run hw_opt < ex/ex2.txt
cabal run hw_opt < ex/ex3.txt
cabal run hw_opt < ex/ex_a1.txt
