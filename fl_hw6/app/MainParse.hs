module Main where

import Parser (parseL)
import Reader (read_st, print_st, pretty_st)

main :: IO ()
main = do
  input <- getContents
  let res = parseL input
  case res of
    Left err -> print err
    Right ex -> do
        putStrLn $ show ex
        putStrLn $ pretty_st ex
