module Main where

import Day1
import System.Environment (getArgs)
import System.IO (hClose, hGetContents, openFile, IOMode (ReadMode))

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["1", "1"] -> dispatch day1_1 "input/day1.txt"
      ["1", "2"] -> dispatch day1_2 "input/day1.txt"
      _ -> error "invalid args"
      where
          dispatch func fileName = do
              handle <- openFile fileName ReadMode
              contents <- hGetContents handle
              putStrLn $ func contents
              hClose handle

