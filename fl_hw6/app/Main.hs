module Main where

import Parser

import Control.Monad.Trans

process :: String -> IO ()
process str = do
  let res = parseL str
  case res of
    Left err -> print err
    Right ex -> print ex

main :: IO ()
main = do
  input <- getContents
  liftIO $ process input
