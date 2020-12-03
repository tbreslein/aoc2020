module Day1
    ( day1_1
    , day1_2
    ) where

import Control.Monad (replicateM)

-- obviously not optimised: this has time complexity of O(n^2), and could be easily be brought down
-- to O(n log(n)), but then this function would not be a one-liner anymore
process :: Int -> [Int] -> Int
process _ [] = 0
process n xs = product . head $ [y | y <- replicateM n xs, sum y == 2020]

day1_1 :: String -> String
day1_1 = show . process 2 . map read . lines

day1_2 :: String -> String
day1_2 = show . process 3 . map read . lines
