module Main where

import Parser (parseL)
import Reader (read_st, print_st)
import Opt (opt)


main :: IO ()
main = do
  input <- getContents
  let res = parseL input
  case res of
    Left err -> print err
    Right ex -> do
        putStrLn $ "input:"
        putStrLn $ input
        putStrLn $ "parsed:"
        putStrLn $ show ex
        putStrLn $ "pretty printed:"
        putStrLn $ print_st ex
        putStrLn $ "optimized:"
        putStrLn $ show $ opt ex
        putStrLn $ "optimized pretty printed:"
        putStrLn $ print_st $ opt ex
