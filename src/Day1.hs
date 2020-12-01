module Day1
    ( day1_1
    , day1_2
    ) where

import Control.Monad (replicateM)

process :: Int -> [Int] -> Int
process _ [] = 0
process n xs = product . head $ [y | y <- replicateM n xs, sum y == 2020]
-- NOTE: replicateM n xs = sequence $ replicate n xs
-- thus, that call produces all n-tuples (well, technically lists, not tuples...), and filters out
-- those that match the sum x == 2020 predicate. By itself, that comprehension would give us a list
-- of all permutations of the list we are actually looking for, so head just takes the first one.

day1_1 :: String -> String
day1_1 = show . process 2 . map read . lines

day1_2 :: String -> String
day1_2 = show . process 3 . map read . lines
