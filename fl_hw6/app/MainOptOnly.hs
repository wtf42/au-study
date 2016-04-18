module Main where

import Parser (parseL)
import Reader (read_st, print_st, pretty_st)
import Opt (opt)


main :: IO ()
main = do
  input <- getContents
  let res = read_st input
  case res of
    Left err -> print err
    Right ex -> do
        putStrLn $ "input:"
        putStrLn $ show ex
        putStrLn $ pretty_st ex
        putStrLn $ "pretty printed:"
        putStrLn $ print_st ex
        putStrLn $ "optimized:"
        putStrLn $ show $ opt ex
        putStrLn $ pretty_st $ opt ex
        putStrLn $ "optimized pretty printed:"
        putStrLn $ print_st $ opt ex
