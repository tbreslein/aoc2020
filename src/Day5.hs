module Day5 (day5_1, day5_2) where

import Data.List (sort)

type Pass = [Char]
type State = (Int, [Pass]) -- basically a ghetto State monad

-- exported functions
day5_1 :: String -> String
day5_1 = show . fst . findBiggestId . (,) 0 . lines

day5_2 :: String -> String
day5_2 = show . findMissingId . lines

-- helper functions
parseToBinary :: Int -> [Char] -> Int
parseToBinary _ [] = 0
parseToBinary l (letter:rest)
    | letter `elem` ['B', 'R'] = 2 ^ l + parseToBinary (l - 1) rest
    | otherwise = parseToBinary (l - 1) rest

-- Task 1 specific
findBiggestId :: State -> State
findBiggestId (curMax, []) = (curMax, [])
findBiggestId (curMax, pass:passes) =
    findBiggestId (max curMax $ parseToBinary 9 pass, passes)

-- Task 2 specific
findMissingId :: [Pass] -> Int
findMissingId = getMissing . sort . map (parseToBinary 9)
    where
        -- identifies the seat by finding the first non-consecutive list entry
        getMissing (x:xs@(y:_)) = if y - x > 1 then y else getMissing xs