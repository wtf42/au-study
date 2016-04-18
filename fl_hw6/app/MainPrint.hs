module Main where

import Reader (read_st, print_st, pretty_st)

main :: IO ()
main = do
  input <- getContents
  let res = read_st input
  case res of
    Left err -> print err
    Right ex -> print $ print_st ex
