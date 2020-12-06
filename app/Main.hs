module Main where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Control.Monad ((<=<))
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["1", "1"] -> dispatch day1_1 (getOutStr args) "input/day1.txt"
      ["1", "2"] -> dispatch day1_2 (getOutStr args) "input/day1.txt"
      ["2", "1"] -> dispatch day2_1 (getOutStr args) "input/day2.txt"
      ["2", "2"] -> dispatch day2_2 (getOutStr args) "input/day2.txt"
      ["3", "1"] -> dispatch day3_1 (getOutStr args) "input/day3.txt"
      ["3", "2"] -> dispatch day3_2 (getOutStr args) "input/day3.txt"
      ["4", "1"] -> dispatch day4_1 (getOutStr args) "input/day4.txt"
      ["4", "2"] -> dispatch day4_2 (getOutStr args) "input/day4.txt"
      ["5", "1"] -> dispatch day5_1 (getOutStr args) "input/day5.txt"
      ["5", "2"] -> dispatch day5_2 (getOutStr args) "input/day5.txt"
      ["6", "1"] -> dispatch day6_1 (getOutStr args) "input/day6.txt"
      ["6", "2"] -> dispatch day6_2 (getOutStr args) "input/day6.txt"
      _ -> error "invalid args"
      where
          dispatch func outStr = putStrLn . (++) outStr . func <=< readFile
          getOutStr args = "Day " ++ head args ++ "; Task " ++ args !! 1 ++ "; Result = "
