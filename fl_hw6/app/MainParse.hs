module Main where

import Parser (parseL)

main :: IO ()
main = do
  input <- getContents
  let res = parseL input
  case res of
    Left err -> print err
    Right ex -> print ex
