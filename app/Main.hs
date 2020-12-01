module Main where

import Day1
import Control.Monad ((<=<))
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["1", "1"] -> dispatch day1_1 "input/day1.txt"
      ["1", "2"] -> dispatch day1_2 "input/day1.txt"
      _ -> error "invalid args"
      where
          dispatch func = putStrLn . func <=< readFile
